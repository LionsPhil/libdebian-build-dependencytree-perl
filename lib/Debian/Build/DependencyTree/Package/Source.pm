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

use warnings;
use strict;

package Debian::Build::DependencyTree::Package::Source;

use Dpkg::Deps qw();

=encoding UTF-8

=head1 NAME

Debian::Build::DependencyTree::Package::Source - Runtime state of ordering algorithm for one control file

=head1 DESCRIPTION

After a C<compute()> call, you can get instances of these objects from a L<Debian::Build::DependencyTree>.

=head1 METHODS

=cut

# Constructor is friend to DependencyTree.
# Takes the Dpkg::Control::Info it is wrapping.
sub _new {
	my ($class, $control) = @_;

	return bless({
			control   => $control,
			builddeps => [], # Array of package name strings
			rundeps   => [], # Union of package name strings from all binaries
			deepdeps  => [], # Package name strings as per _deependeps()
	}, $class);
}

# Friend to DependencyTree.
# Compute the dependencies of this package, returning warnings.
# Takes a reference to a binary->source name lookup hash, which it will populate,
# the name-to-Binary object map for binaries, which it will also populate, and
# the host and build architectures, which can be undefined.
# Must be performed after construction, before use.
sub _parsedeps {
	my ($self, $dependencytree, $binary_to_source, $binary_packages) = @_;
	my @warnings;

	my $source_control = $self->{control}->get_source();
	my $source_name    = $source_control->{source};

	$self->{builddeps} = [$dependencytree->flatten_dependency_line(
		$source_control->{'build-depends'} // '', $source_name, \@warnings, 1)];

	my %rundeps;

	foreach my $binary_control ($self->{control}->get_packages()) {
		my $binary_name = $binary_control->{package};

		if(exists $binary_packages->{$binary_name}) {
			# Hang on, we've already encountered a binary called this...
			push @warnings, Debian::Build::DependencyTree::Warning::MultipleSources->new(
				using => $binary_to_source->{$binary_name}, alternative => $source_name);
		} else {
			# Record the binary as belong to this package, and represent it
			my $binary_object = Debian::Build::DependencyTree::Package::Binary->_new($binary_control);
			push @warnings, $binary_object->_parsedeps($dependencytree);
			$binary_packages->{$binary_name}  = $binary_object;
			$binary_to_source->{$binary_name} = $source_name;

			# Bring the rundeps up into this package, using a hash to union them
			foreach my $rundep ($binary_object->rundeps()) {
				$rundeps{$rundep} = 1;
			}
		}
	}

	$self->{rundeps} = [sort keys %rundeps];

	return @warnings;
}

# Friend to DependencyTree.
# Compute the deep dependencies of this package, assuming all packages are now parsed.
# Takes a reference to hash mapping from names to Package::Binary objects.
# Returns warnings.
sub _deependeps {
	my ($self, $binary_by_name) = @_;

	my @warnings;
	my %deepdeps;
	my @consideration;

	# Start by adding the immediate build dependencies to the packages under
	# consideration.
	@consideration = @{$self->{builddeps}};

	# Do a breadth-first, non-revisiting recursion down the runtime dependencies
	# of packages under consideration, so we don't chase our tail.
	my %binary_lookup_failed;
	while(@consideration) {
		my $consider = pop @consideration;

		unless($deepdeps{$consider}) {
			if(exists $binary_by_name->{$consider}) {
				$deepdeps{$consider} = 1;
				push @consideration, $binary_by_name->{$consider}->rundeps();
			} else {
				unless($binary_lookup_failed{$consider}) {
					push @warnings, Debian::Build::DependencyTree::Warning::MissingDependency->new(
						package => $self->{control}->get_source()->{source},
						dependency => $consider);
					$binary_lookup_failed{$consider} = 1;
				}
			}
		}
	}

	$self->{deepdeps} = [keys %deepdeps];

	return @warnings;
}

=head2 control()

Get the L<Dpkg::Control::Info> this object represents the source package of.

=cut

sub control   { (shift)->{control} }

=head2 builddeps()

Get a list of the binary package names of this package's build dependencies.

=cut

sub builddeps { @{(shift)->{builddeps}} }

=head2 rundeps()

Get a list of the binary package names of the I<immediate> runtime dependencies of this package.

=cut

sub rundeps   { @{(shift)->{rundeps}  } }

=head2 deepdeps()

Get a list of binary package names needed to build this source.
The runtime dependencies of a package are not necessary to build it, but the build dependencies, and the full, recursive set of I<their> runtime dependencies, are.

=cut

sub deepdeps  { @{(shift)->{deepdeps} } }

=head2 binaries()

Get a list of the binary package names this package provides.
This includes any virtual packages.

=cut

sub binaries  { @{(shift)->{binaries} } }

1;
