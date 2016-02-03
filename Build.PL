#!/usr/bin/perl
use warnings;
use strict;

use Module::Build;

Module::Build->new(
	module_name    => 'Debian::Build::DependencyTree',
	license        => 'GPL_2',
	build_requires => {
		'File::Slurp' => 0,
		'File::Temp'  => 0,
		'Test::Spec'  => 0,
	},
	requires       => {
		# The Dpkg::* modules are provided on Debian systems by libdpkg-perl.
		# They are not declared here because this can confuse build systems
		# (TravisCI in particular) that expect to be able to feed all of these
		# to cpanm; the modules are *not* on CPAN so it would fail.
		#'Dpkg::Control::Info' => 0,
		#'Dpkg::Deps'          => 0,
		#'Dpkg::Substvars'     => 0,
		'Exception::Class'    => 0,
		'Try::Tiny'           => 0,
	},
)->create_build_script();
