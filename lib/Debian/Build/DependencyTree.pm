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

package Debian::Build::DependencyTree;
use     Debian::Build::DependencyTree::Package::Source;
use     Debian::Build::DependencyTree::Package::Binary;
use     Debian::Build::DependencyTree::Exception;

use Dpkg::Deps;
use Dpkg::Substvars;

=encoding UTF-8

=head1 NAME

Debian::Build::DependencyTree - Analyze the build dependencies between packages

=head1 SYNOPSIS

  my $deptree = Debian::Build::DependencyTree->new(
      packages => @dpkg_controls,
      roots => qw(build-essential debhelper),
  );

  $deptree->compute();

  my @warnings = $deptree->warnings();
  warn(join("\n", @warnings)) if @warnings;

  foreach my $stage ($deptree->stages()) {
      parallel_build(@$stage);
  }

=head1 METHODS

=head2 new(%options)

Constructs and prepares to compute a dependency tree of packages.

Takes a hash of options:

=over 4

=item packages

An arrayref of L<Dpkg::Control::Info> objects for the packages being considered.

=item roots

An arrayref of source package names that are to be considered bootstrapped, to break loops.
For example, you probably want to list C<debhelper> here if it's in C<packages>.
These will also be forcefully promoted to the first stage of dependencies if present in C<packages>.

=item substvars

A L<Dpkg::Substvars> object used to replace substitution variables in dependencies.
One will be default-constructed if omitted.

This is necessary if operating on control files read from unbuilt debhelper package sources, but obviously increases the amount of assumptions the ordering makes.
You will probably want to C<set_as_used()> in the object mappings for the C<:Depends> variables created by the likes of L<dpkg-shlibdeps> and L<dh_perl>, preventing them being mistaken for missing packages at the cost of ordering accuracy.

=item hostarch, buildarch

Define the host and build architectures for resolving dependencies.
If undefined, defers to C<Dpkg::Arch::get_host/build_arch()>.

=item scorelimit

How many rounds of build order computation to allow before deciding it is stuck in a loop.
Defaults to a sensibly large number; you should not normally need to adjust this.

=back

=cut

sub new {
	my ($class, %options) = @_;

	return bless({
			controls   => $options{packages}  // [],
			roots      => $options{roots}     // [],
			substvars  => $options{substvars} // Dpkg::Substvars->new(),
			hostarch   => $options{hostarch},
			buildarch  => $options{buildarch},
			scorelimit => $options{scorelimit} // 100,

			warnings         => [],
			source_packages  => {},
			binary_packages  => {},
			binary_to_source => {},
			scores           => {},
			stages           => undef,
	}, $class);
}

=head2 compute()

Run the algorithm to derive a build ordering for the packages.

If it fails, throws a L<Debian::Build::DependencyTree::Error> (see L<Debian::Build::DependencyTree::Exception>).
There is a deliberately weak exception guarantee: a thrown error may still have set C<warnings()>, which can be useful to identify the conditions leading to the error, and provided data for C<source_packages()> and friends.

=cut

sub compute {
	my ($self) = @_;

	# Mapping from source package name to internal Package object
	my %source_packages;

	# Mapping from binary package name to internal Package object
	my %binary_packages;

	# Mapping from binary package name to source package name which provides it
	my %binary_to_source;

	# Quick lookup hash for root packages by name
	my %is_root;

	foreach my $root (@{$self->{roots}}) {
		$is_root{$root} = 1;
	}

	# Build Package::Source objects for our control files, including parsing and
	# computing their immediate dependencies
	foreach my $control (@{$self->{controls}}) {
		my $package = $control->get_source()->{'source'};

		$source_packages{$package} =
			Debian::Build::DependencyTree::Package::Source->_new($control);

		# Work out the immediate dependencies and provided binaries
		push @{$self->{warnings}}, $source_packages{$package}->_parsedeps(
			$self, \%binary_to_source, \%binary_packages);
	}

	# Work out the deeper dependencies (follow runtime requirements)
	foreach my $package (keys %source_packages) {
		push @{$self->{warnings}}, $source_packages{$package}->_deependeps(\%binary_packages);
	}

	# Stash the dependency analysis results now they are complete, before we get
	# to the state of the computation which may throw.
	$self->{source_packages}  = \%source_packages;
	$self->{binary_packages}  = \%binary_packages;
	$self->{binary_to_source} = \%binary_to_source;

	# Compute scores for each package based on those that depend on it,
	# iterating until the graph is stable.

	# Initialize the scores
	my %package_scores;
	foreach my $package (keys %source_packages) {
		$package_scores{$package} = 0;
	}

	# Propagate scores out across dependency graph edges
	my $changed;
	my $highscore = 0;
	do {
		$changed = 0;
		my %new_scores = %package_scores;

		foreach my $package_name (keys %source_packages) {
			my $package_object = $source_packages{$package_name};

			# If this package is a root, it should *not* propagate its score to
			# its dependencies. If debhelper is a root, things depending on
			# debhelper should make dephelper order early, but debhelper should
			# not make its dependency, Perl, try to order even earlier.
			if($is_root{$package_name}) { next; }

			# Anything we depend on should have a score higher than ours
			my $propagated_score = $package_scores{$package_name} + 1;

			foreach my $dependency ($package_object->deepdeps()) {
				# Find the *source* package for this *binary* dependency
				my $source_dependency = $binary_to_source{$dependency};

				# Assertion: $source_dependency must be defined, since the
				# deepdeps() we are iterating over would have skipped the
				# entry (and raised a MissingDependency warning) if the
				# binary was unknown (i.e. we didn't have its source).

				# Assertion: $new_scores{$source_dependency} must be
				# defined, since the set of values in there matches is at
				# most a subset of the set of source packages which was used
				# to initialize the scores.

				if($new_scores{$source_dependency} < $propagated_score) {
					# Our dependency's score is lower than the one we could
					# propagate to it. Increase it and mark the graph as not
					# yet settled.
					$new_scores{$source_dependency} = $propagated_score;
					$changed = 1;
					if($propagated_score > $highscore)
						{ $highscore = $propagated_score; }
				} else {
					# Our dependency's score is already at or ahead of where
					# we'd put it. Nothing to do.
				}
			}
		}

		# Switch over to the new scores, and update the member reference to
		# point to them so it's correct even if we are about to throw.
		%package_scores = %new_scores;
		$self->{scores} = \%package_scores;

		# Detect if the graph isn't settling, which indicates a loop
		if($highscore == $self->{scorelimit}) {
			Debian::Build::DependencyTree::Error::Loop->throw();
		}

	} while($changed);

	# Force the scores of all root packages to the highscore, so they get into
	# the first build stage, if they were provided as packages to build.
	foreach my $root (@{$self->{roots}}) {
		if(exists $package_scores{$root}) {
			$package_scores{$root} = $highscore;
		}
	}

	# Use the scores to group the packages into stages
	my %score_to_packages;
	foreach my $package (keys %package_scores) {
		my $score = $package_scores{$package};
		$score_to_packages{$score} //= [];
		push @{$score_to_packages{$score}}, $package;
	}

	my @stages;
	foreach my $score (sort {$b <=> $a} keys %score_to_packages) {
		push @stages, [ sort @{$score_to_packages{$score}} ];
	}

	$self->{stages} = \@stages;
}

=head2 warnings()

Returns a list of warnings encountered during C<compute()>.
See the L<Debian::Build::DependencyTree::Warning> classes under L<Debian::Build::DependencyTree::Exception>.
These vary in severity; ideally the list should be empty.

=cut

sub warnings {
	return @{$_[0]->{warnings}};
}

=head2 stages()

Return an array of stages needed to build all of the provided packages in the correct order.
Each stage is an arrayref of the source package names in that stage.

Will throw if C<compute()> has not been called, or if it was unable to determine an ordering.

=cut

sub stages {
	my ($self) = @_;

	if(defined $self->{stages}) {
		return @{$self->{stages}};
	} else {
		die "stages() called without a successful compute()";
	}
}

=head2 source_packages(), binary_packages()

Returns a hash (not a hashref) from names to L<Debian::Build::Package::Source> or L<Debian::Build::Package::Binary> objects respectively.
This can be useful for diagnostics.

The hash will be empty until C<compute()> has been called.

=cut

sub source_packages  { return %{$_[0]->{source_packages} }; }
sub binary_packages  { return %{$_[0]->{binary_packages} }; }

=head2 binary_to_source()

Returns a hash (not a hashref) from binary package names to the name of the source package which builds that source package, as understood by DependencyTree.
This can be useful for diagnostics.

The hash will be empty until C<compute()> has been called.

=cut

sub binary_to_source { return %{$_[0]->{binary_to_source}}; }

=head2 scores()

Returns a hash (not a hashref) from source package names to the scores in the ordering algorithm.
Higher scores mean earlier ordering.
This can be useful for diagnostics.

The hash will be empty until C<compute()> has been called.

=cut

sub scores           { return %{$_[0]->{scores}          }; }

=head2 loops()

If C<compute()> threw an C<Error::Loop>, this tries to find loops which may have caused it.
Returns a list of arrayrefs, each of which is a sequence of hashrefs with C<source> and C<binary> package names that forms a dependency loop returning back to the first.
(The first entry have an undefined C<binary> name, since it is the source package trying to be built.)
Note that you will may get the same loop expressed several times, with different starting points within it.

You can combine this information with the C<deepdeps()> of the C<source_packages()> and C<binary_to_source()> to visualize the problem.

=cut

sub loops {
	my ($self) = @_;
	my @loops;

	# Find the set of packages which are even involved in loops; this will
	# include some innocent bystanders which are dependencies of the problem
	# packages.
	my @loopers = sort
		grep { $self->{scores}->{$_} == $self->{scorelimit} }
		keys %{$self->{scores}};

	# For each suspect, follow its dependencies and see if we get back to it
	foreach my $suspect (@loopers) {
		# Source packages we've visited before
		my %visited;
		# Trail of steps we took to get here
		my @steps;

		# Try taking a step along a possible loop. Takes copies of arguments,
		# so the stack branches them for us. Writes loops to @loops as a
		# side-effect.
		my $take_step = sub {
			my ($take_step, $visited_r, $steps_r, $source, $binary) = @_;
			my %visited = %$visited_r;
			my @steps   = @$steps_r;

			# This can happen if we don't know which source provides a binary
			# we depend on somewhere.
			return unless defined $source;

			# Record this step
			push @steps, {source => $source, binary => $binary};

			if($visited{$source}) {
				# Was the previous step the same source?
				# This can happen when e.g. a dev binary package depends on a
				# normal library binary package, and isn't a loop since both
				# will be provided by the same build of the source.
				if($steps[$#steps - 1]->{source} eq $source) {
					# It's not a loop (yet, anyway), but don't recurse, since
					# we'll be considering the same source over and over again.
					# Covering any other binaries is handled by the breadth of
					# the recursion.
				} else {
					# We've found a loop, currently in @steps!
					push @loops, \@steps;
				}
			} else {
				# Remember we've been here before
				$visited{$source} = 1;

				# Try every build dependency of the source package
				my $source_package = $self->{source_packages}->{$source};
				foreach my $build_dep (sort $source_package->builddeps()) {
					$take_step->($take_step,
						\%visited, \@steps,
						$self->{binary_to_source}->{$build_dep}, $build_dep);
				}

				if(defined $binary) {
					# Try every runtime dependency of the package
					my $binary_package = $self->{binary_packages}->{$binary};
					foreach my $run_dep (sort $binary_package->rundeps()) {
						$take_step->($take_step,
							\%visited, \@steps,
							$self->{binary_to_source}->{$run_dep}, $run_dep);
					}
				}
			}
		};

		# Take this suspect as the first step
		$take_step->($take_step, \%visited, \@steps, $suspect, undef);
	}

	return @loops;
}

=head2 flatten_dependency_line($dependency_line, $warnings, $is_build_dep)

Utility method to flatten a L<Dpkg::Deps> structure down into a flat list of
of package names.

Takes the string of the dependency line, the package name (for diagnostics), an
arrayref to add warnings to, and a boolean indicating if this is a
C<Build-Depends> line.

=cut

sub flatten_dependency_line {
	my ($self, $dependency_line, $package, $warnings, $is_build_dep) = @_;

	my $hostarch  = $self->{hostarch};
	my $buildarch = $self->{buildarch};

	# Apply variable substitutions
	$dependency_line = $self->{substvars}->substvars($dependency_line);

	# Parse the dependency line
	my %deps_options = (
		use_arch            => 1,
		use_profiles        => 1,
		reduce_arch         => 1,
		reduce_profiles     => 1,
		reduce_restrictions => 1,
	);
	$deps_options{'host_arch' } = $hostarch  if defined $hostarch;
	$deps_options{'build_arch'} = $buildarch if defined $buildarch;
	$deps_options{'build_dep' } = 1          if $is_build_dep;

	my $deps = Dpkg::Deps::deps_parse($dependency_line, %deps_options);
	# We can't use simplify_deps here because we don't know what binaries are
	# covered by roots for KnownFacts; only what sources.

	# Flatten the dependency parse tree
	unless(defined $deps) {
		push @$warnings, Debian::Build::DependencyTree::Warning::DependenciesUnparseable->new(
			package => $package, dependencies => $dependency_line);
		return @{[]};
	}

	my $recursive_flatten = sub {
		my ($recursive_flatten, $deps) = @_;
		if($deps->isa('Dpkg::Deps::Simple')) {

			my $had_relation = !!(defined $deps->{relation});
			my $had_arches   = !!(defined $deps->{arches});
			my $had_archqual = !!(defined $deps->{archqual});
			if($had_relation || $had_arches || $had_archqual) {
				push @$warnings, Debian::Build::DependencyTree::Warning::IgnoringDependencyQualifiers->new(
					package => $package, relation => $had_relation, arches => $had_arches, archqual => $had_archqual);
			}

			my $package = $deps->{package};
			return (defined $package ? @{[$package]} : ());

		} elsif($deps->isa('Dpkg::Deps::OR')) {

			my ($first, @rest) = @{$deps->{list}};
			push @$warnings, Debian::Build::DependencyTree::Warning::AssumingFirstDependency->new(
				package => $package, used => $first, ignored => \@rest);
			return (defined $first ? $recursive_flatten->($recursive_flatten, $first) : ());

		} else {

			my @deps;
			foreach my $dep ($deps->get_deps()) {
				push @deps, $recursive_flatten->($recursive_flatten, $dep);
			}
			return @deps;
		}
	};
	my @flat_dependencies = $recursive_flatten->($recursive_flatten, $deps);

	return @flat_dependencies;
}


=head1 NOTES

This class makes use of the C<Dpkg::*> modules by Raphaël Hertzog E<lt>hertzog@debian.orgE<gt>, which are installed on Debian-derived distributions as part of C<libdpkg-perl>.
As far as I know they are not published to CPAN for use on other systems.

=cut

1;
