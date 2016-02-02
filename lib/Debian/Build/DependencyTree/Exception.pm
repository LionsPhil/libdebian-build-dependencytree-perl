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

=encoding UTF-8

=head1 NAME

Debian::Build::DependencyTree::Exception - Exceptions return/thrown by DependencyTree

=head1 CLASSES

All warnings and errors use L<Exception::Class>, so are objects.
You can programatically distinguish them with C<isa>.

These types are relative to C<Debian::Build::DependencyTree>.

=cut

# Internal base class for all exception behaviour
package Debian::Build::DependencyTree::Exception;
use parent qw(Exception::Class::Base);

# Use the description with fields for the full message, should someone print one
sub full_message {
	my ($self) = @_;
	my $message = $self->description();

	# Annoyingly there doesn't appear to be a way to introspect fields here and
	# include them. So do a very, very naff thing, and hardcode our known
	# possible fields. (Even digging into the guts of the implementation, fields
	# are put flat into the object, mixed in with internal data.)
	my @field_texts;
	foreach my $field (qw(package dependencies relation arches archqual used ignored alternative dependency)) {
		if(exists $self->{$field}) {
			my $value = $self->{$field};
			if(ref $value eq 'ARRAY') { $value = join(', ', @$value); }
			push @field_texts, "$field => '$value'";
		}
	}

	if(@field_texts) {
		$message .= ' ('.(join ', ', @field_texts).')';
	}

	# (Can add $self->trace()->as_string() here for very verbose debugging)
	return $message;
}

package Debian::Build::DependencyTree::Warning;
use parent -norequire, qw(Debian::Build::DependencyTree::Exception);

=head2 Warning::DependenciesUnparseable

The dependencies of C<package>, source or binary, could not be parsed by L<Dpkg::Deps>.
The dependency line I<after> variable substitution is in C<dependencies>.

=head2 Warning::IgnoringDependencyQualifiers

The dependencies of C<package> include some qualifiers that DependencyTree is not smart enough to respect.
C<relation> (of versions), C<arches>, and C<archqual> are booleans indicating which types of qualifiers were ignored, per the properties of a L<Dpkg::Deps::Simple>.
This is normally unavoidable for nontrivial packages, but harmless unless it causes incorrect dependency selection.

=head2 Warning::AssumingFirstDependency

There were multiple packages in an alternation available to satisfy the dependencies of C<package>.
DependencyTree has chosen to use C<used>, and an arrayref of the others is in C<ignored>.

=head2 Warning::MultipleSources

Multiple sources have been found providing the same binary package.
This warning is raised for every extra source package.
The C<using> field contains the source package name that DependencyTree will use for build ordering, and C<alternative> the one it is ignoring.

=head2 Warning::MissingDependency

The source package C<package> has a deep dependency, possibly via runtimes of its build dependencies, on binary package C<dependency>.
If you are passing DependencyTree subsets of a complete system, this is normal.
If you are passing it a complete distribution, this should be considered fatal.

=cut

use Exception::Class (
	'Debian::Build::DependencyTree::Warning::DependenciesUnparsable' => {
		isa => 'Debian::Build::DependencyTree::Warning',
		description => 'could not parse the dependencies of a package',
		fields => ['package', 'dependencies'],
	},
	'Debian::Build::DependencyTree::Warning::IgnoringDependencyQualifiers' => {
		isa => 'Debian::Build::DependencyTree::Warning',
		description => 'ignoring some qualifiers on dependencies',
		fields => ['package', 'relation', 'arches', 'archqual'],
	},
	'Debian::Build::DependencyTree::Warning::AssumingFirstDependency' => {
		isa => 'Debian::Build::DependencyTree::Warning',
		description => 'assuming the first of a set of possible dependencies will be used',
		fields => ['package', 'used', 'ignored'],
	},
	'Debian::Build::DependencyTree::Warning::MultipleSources' => {
		isa => 'Debian::Build::DependencyTree::Warning',
		description => 'multiple sources provide the same binary package',
		fields => ['using', 'alternative'],
	},
	'Debian::Build::DependencyTree::Warning::MissingDependency' => {
		isa => 'Debian::Build::DependencyTree::Warning',
		description => 'package depends on an unknown binary package',
		fields => ['package', 'dependency'],
	},
);

package Debian::Build::DependencyTree::Error;

=head2 Error::Loop

There is a dependency loop between the packages, which means it's not possible to give a deterministic ordering for how to build them (it is not possible to bootstrap them).

There are several possible causes for this:

=over 4

=item DependencyTree has made bad assumptions about build dependencies

This class is not C<apt-get>, and does not consider all possible ways to satisfy the dependencies of a packages.
It is possible the one it selected has loops where another would not.
Warnings are generated every time an assumption is made, except for the C<substvars> provided on construction.

=item Some of the base packages need bootstrapping

Packages such as C<debhelper> have nontrivial dependencies (e.g. C<perl>) but are themselves dependencies of basically every package.
To break this loop, you must bootstrap your build environment, and tell DependencyTree which ones you have bootstrapped via the C<roots> constructor argument.

=item There is a non-bootstrap build loop in the dependencies

This is probably a bug with the packages involved, since they can no longer be built from source from a clean slate.

=back

=cut

use Exception::Class (
	'Debian::Build::DependencyTree::Error' => {
		isa => 'Debian::Build::DependencyTree::Exception',
	},
	'Debian::Build::DependencyTree::Error::Loop' => {
		isa => 'Debian::Build::DependencyTree::Error',
		description => 'build dependencies contain loops; no deterministic ordering is possible',
	},
);

1;
