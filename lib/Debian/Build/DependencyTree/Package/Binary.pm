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

package Debian::Build::DependencyTree::Package::Binary;

=encoding UTF-8

=head1 NAME

Debian::Build::DependencyTree::Package::Binary - Runtime state of ordering algorithm for derived packages

=head1 DESCRIPTION

After a C<compute()> call, you can get instances of these objects from a L<Debian::Build::DependencyTree>.

=head1 METHODS

=cut

# Constructor is friend to Source package.
# Takes the Dpkg::Control (_not_ ::Info) for the binary package it is wrapping.
sub _new {
	my ($class, $control) = @_;

	return bless({
			control   => $control,
			rundeps   => [],
	}, $class);
}

# Friend to Source package.
# Compute the dependencies of this package, returning warnings.
# Must be performed after construction, before use.
sub _parsedeps {
	my ($self, $dependencytree) = @_;
	my @warnings;

	$self->{rundeps} = [$dependencytree->flatten_dependency_line(
		$self->{control}->{depends} // '', $self->{control}->{package}, \@warnings, 0)];

	# If we *have* pre-dependencies, include them too
	my $predepends = $self->{control}->{'pre-depends'} // '';
	if($predepends ne '') {
		push @{$self->{rundeps}}, $dependencytree->flatten_dependency_line(
			$predepends, $self->{control}->{package}, \@warnings, 0);
	}

	return @warnings;
}

=head2 control()

Get the L<Dpkg::Control> this object represents.
(This will be one of the packages within the L<Dpkg::Control::Info> of its source package.)

=cut

sub control   { (shift)->{control} }

=head2 rundeps()

Get a list of the binary package names of the I<immediate> runtime dependencies of this package.

=cut

sub rundeps   { @{(shift)->{rundeps}  } }

1;
