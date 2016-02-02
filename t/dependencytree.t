#!/usr/bin/perl
# Debian::Build::DependencyTree - compute a build order for Debian packages
# Copyright (c) 2016 Smoothwall Ltd.
#
# This program is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License
# as published by the Free Software Foundation; either version 2
# of the License, or (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.

use strict;
use warnings;

use Test::Spec;

use Dpkg::Control::Info;
use Dpkg::Substvars;
use File::Temp qw();
use File::Slurp;

describe 'DependencyTree' => sub {
	my $tempdir_handle;
	my $tempdir;
	my %control_infos;

	before all => sub {
		# Test data control files
		# Dpkg::Control::Info's slightly naff API means we have to write these
		# as tempfiles; it won't even read from an IO::Scalar, since it open()s
		# a path.
		$tempdir_handle = File::Temp->newdir('dependency-tree-tests-XXXXXXXX',
			TMPDIR => 1, CLEANUP => 1);
		$tempdir = $tempdir_handle->dirname();

		# A <- - - B <----- C #
		#  ,------7           # Dashed: runtime dependency
		# D <- - - E <----- F # Solid:    build dependency
		#  `---------------7  #
		#
		# Some things depend on debhelper for 'realism' (and to avoid an empty
		# build-deps); otherwise not to cut down warning generation.

		write_file("$tempdir/a", q{
Source: a
Section: main
Priority: standard
Build-Depends: debhelper (>= 9)

Package: a1
Architecture: all
Depends:
Description: Standalone package
}		);
		write_file("$tempdir/b", q{
Source: b
Section: main
Priority: standard
Build-Depends: debhelper

Package: b1
Architecture: all
Depends: a1, ${testing:Depends}
Description: Standalone build-deps, one run-dep, one substvar
}		);
		write_file("$tempdir/c", q{
Source: c
Section: main
Priority: standard
Build-Depends: b1

Package: c1
Architecture: all
Depends:
Description: Build-depends on a package with a run-depend
}		);
		write_file("$tempdir/d", q{
Source: d
Section: main
Priority: standard
Build-Depends: b1, f1

Package: d1
Architecture: all
Depends:
Description: Build-depends on two packages
}		);
		write_file("$tempdir/e", q{
Source: e
Section: main
Priority: standard
Build-Depends: debhelper

Package: e1
Architecture: all
Depends: d1
Description: Causes a loop with D and F
}		);
		write_file("$tempdir/f", q{
Source: f
Section: main
Priority: standard
Build-Depends: e1

Package: f1
Architecture: all
Depends:
Description: Build-dep and build-depped upon, complex build-dep constraints
}		);
		write_file("$tempdir/g", q{
Source: g
Section: main
Priority: standard
Build-Depends: h1

Package: g1
Architecture: all
Depends:
Description: Member of a trivial build dependency loop
}		);
		write_file("$tempdir/h", q{
Source: h
Section: main
Priority: standard
Build-Depends: g1

Package: h1
Architecture: all
Depends:
Description: Member of a trivial build dependency loop
}		);

		foreach my $package (qw(a b c d e f g h)) {
			$control_infos{$package} = Dpkg::Control::Info->new("$tempdir/$package");
		}
	};

	it 'should be usable' => sub {
		use_ok('Debian::Build::DependencyTree');
	};

	describe 'when used' => sub {
		my $substvars;

		# Like cmp_bag, but understanding our exceptions. Only handles
		# arrayrefs of DependencyTree exceptions in $got, and expects an
		# arrayref of hashrefs in $expected, with a 'type' key, and additional
		# expected fields.
		my $check_warnings = sub {
			my ($got, $expected) = @_;
			my @failures;

			# Note this isn't an operator==; it takes the 'expected' structure
			# on the RHS.
			my $exceptions_are_same = sub {
				# Check type
				my ($got, $expected) = @_;
				my $got_type = ref $got;
				$got_type =~ s/.*:://;
				return 0 unless $got_type eq $expected->{type};

				# Check expected fields are present with expected values
				foreach my $field (keys %$expected) {
					next if $field eq 'type';
					my $got_value      = $got->$field();
					my $expected_value = $expected->{$field};
					# Poor man's cmp_bag for simple cases
					if(ref $expected_value eq 'ARRAY') {
						return 0 unless
							(join "\0", (sort @$got_value)) eq
							(join "\0", (sort @$expected_value));
					} else {
						return 0 unless $got_value eq $expected_value;
					}
				}
				return 1;
			};

			# Copy expected exceptions into a hash for easy removal, so we can
			# detect ones we didn't get.
			my %expected_leftover;
			{
				my $id = 0;
				foreach my $exception_specification (@$expected) {
					$expected_leftover{++$id} = $exception_specification;
				}
			}

			foreach my $got_exception (@$got) {
				# Is this exception expected?
				my $found = 0;
				foreach my $id (keys %expected_leftover) {
					my $exception_specification = $expected_leftover{$id};
					if($exceptions_are_same->($got_exception, $exception_specification)) {
						delete $expected_leftover{$id};
						$found = 1;
						last;
					}
				}

				unless($found) {
					push @failures, "unexpected exception '$got_exception'";
				}
			}

			# Are there any exceptions we were expecting we didn't get?
			foreach my $exception_specification (values %expected_leftover) {
				# Re-implementing exception stringification here would be evil;
				# use explain() and accept slightly uglier output.
				# For some exciting reason it only works in list context; in
				# scalar it helpfully returns '1'.
				my ($spec) = explain $exception_specification;
				$spec =~ s/\n//g;
				push @failures, "missing exception '$spec'";
			}

			ok(!@failures) or diag join(', ', @failures);
		};

		before each => sub {
			$substvars = Dpkg::Substvars->new();
			$substvars->set_as_used('testing:Depends', '');
		};

		it 'can flatten dependency lines' => sub {
			my @warnings;
			my $dt = Debian::Build::DependencyTree->new();
			cmp_bag([$dt->flatten_dependency_line(
				'a | b, c [amd64], d [!amd64], e (>= 5)',
				'testpackage', \@warnings, 1)],
				['a', 'c', 'e']);
			$check_warnings->(\@warnings, [
				# 'a | b' should pick 'a' and warn that it chose it over 'b'
				{type => 'AssumingFirstDependency',      package => 'testpackage', used => 'a', ignored => [qw(b)]},
				# We don't have the information to quash the version relation, but we *do* know which arch we are
				{type => 'IgnoringDependencyQualifiers', package => 'testpackage', relation => 1, arches => '', archqual => ''},
			]);
		};

		it 'computes simple dependencies' => sub {
			my $dt = Debian::Build::DependencyTree->new(
				packages  => [$control_infos{b}, $control_infos{c}],
				substvars => $substvars,
			);
			$dt->compute();
			$check_warnings->([$dt->warnings()], [
				{type => 'MissingDependency',            package => 'b', dependency => 'debhelper'},
				# This is that 'a' is not in packages but 'b' has a runtime dep on it that 'c' needs to build
				{type => 'MissingDependency',            package => 'c', dependency => 'a1'},
			]);
			is_deeply([$dt->stages()], [
				[qw(b)],
				[qw(c)],
			]) or diag explain [$dt->stages()];
		};

		it 'computes runtime-only dependencies as parallelizable' => sub {
			my $dt = Debian::Build::DependencyTree->new(
				packages  => [$control_infos{a}, $control_infos{b}],
				substvars => $substvars,
			);
			$dt->compute();
			$check_warnings->([$dt->warnings()], [
				# This is the debhelper version requirement
				{type => 'IgnoringDependencyQualifiers', package => 'a', relation => 1, arches => '', archqual => ''},
				{type => 'MissingDependency',            package => 'a', dependency => 'debhelper'},
				{type => 'MissingDependency',            package => 'b', dependency => 'debhelper'},
			]);
			cmp_deeply([$dt->stages()], [
				[qw(a b)],
			]);
		};

		it 'computes a mix of run- and build-time dependencies' => sub {
			my $dt = Debian::Build::DependencyTree->new(
				packages  => [$control_infos{a}, $control_infos{b}, $control_infos{c}],
				substvars => $substvars,
			);
			$dt->compute();
			$check_warnings->([$dt->warnings()], [
				{type => 'IgnoringDependencyQualifiers', package => 'a', relation => 1, arches => '', archqual => ''},
				{type => 'MissingDependency',            package => 'a', dependency => 'debhelper'},
				{type => 'MissingDependency',            package => 'b', dependency => 'debhelper'},
			]);
			cmp_deeply([$dt->stages()], [
				[qw(a b)],
				[qw(c)],
			]);
		};

		it 'computes a more complicated graph of dependencies' => sub {
			my $dt = Debian::Build::DependencyTree->new(
				packages  => [$control_infos{a}, $control_infos{b}, $control_infos{c},
					$control_infos{d}, $control_infos{f}],
				substvars => $substvars,
			);
			$dt->compute();
			$check_warnings->([$dt->warnings()], [
				{type => 'IgnoringDependencyQualifiers', package => 'a', relation => 1, arches => '', archqual => ''},
				{type => 'MissingDependency',            package => 'a', dependency => 'debhelper'},
				{type => 'MissingDependency',            package => 'b', dependency => 'debhelper'},
				{type => 'MissingDependency',            package => 'f', dependency => 'e1'},
			]);
			cmp_deeply([$dt->stages()], [
				[qw(a b f)],
				[qw(c d)],
			]);
		};

		it 'identifies trivial loops' => sub {
			my $dt = Debian::Build::DependencyTree->new(
				packages  => [$control_infos{g}, $control_infos{h}],
			);
			trap { $dt->compute(); };
			$check_warnings->([$dt->warnings()], []);
			isa_ok($trap->die(), 'Debian::Build::DependencyTree::Error::Loop');
		};

		it 'identifies loops involving runtime dependencies in complex graphs' => sub {
			my $dt = Debian::Build::DependencyTree->new(
				packages  => [$control_infos{a}, $control_infos{b}, $control_infos{c},
					$control_infos{d}, $control_infos{e}, $control_infos{f}],
				substvars => $substvars,
			);
			trap { $dt->compute(); };
			$check_warnings->([$dt->warnings()], [
				{type => 'IgnoringDependencyQualifiers', package => 'a', relation => 1, arches => '', archqual => ''},
				{type => 'MissingDependency',            package => 'a', dependency => 'debhelper'},
				{type => 'MissingDependency',            package => 'b', dependency => 'debhelper'},
				{type => 'MissingDependency',            package => 'e', dependency => 'debhelper'},
			]);
			isa_ok($trap->die(), 'Debian::Build::DependencyTree::Error::Loop');
			# Have we identified which packages have got caught up in this loop?
			my @loops = $dt->loops();
			cmp_deeply(\@loops, [
				[
					{ source => 'd', binary => undef },
					{ source => 'f', binary => 'f1' },
					{ source => 'e', binary => 'e1' },
				],
				[
					{ source => 'f', binary => undef },
					{ source => 'e', binary => 'e1' },
					{ source => 'd', binary => 'd1' },
				],
				# e -> d doesn't loop around to f, since e -> d is only a rundep
			]) or diag explain \@loops;
		};

		it 'breaks loops if a package is bootstrapped' => sub {
			my $dt = Debian::Build::DependencyTree->new(
				packages  => [$control_infos{a}, $control_infos{b}, $control_infos{c},
					$control_infos{d}, $control_infos{e}, $control_infos{f}],
				roots     => [qw(d e)],
				substvars => $substvars,
			);
			$dt->compute();
			$check_warnings->([$dt->warnings()], [
				{type => 'IgnoringDependencyQualifiers', package => 'a', relation => 1, arches => '', archqual => ''},
				{type => 'MissingDependency',            package => 'a', dependency => 'debhelper'},
				{type => 'MissingDependency',            package => 'b', dependency => 'debhelper'},
				{type => 'MissingDependency',            package => 'e', dependency => 'debhelper'},
			]);
			cmp_deeply([$dt->stages()], [
				[qw(a b d e)],
				[qw(c f)],
			]);
		};
	};
};

runtests unless caller;
