#!/usr/bin/perl
use warnings;
use strict;

use Module::Build;

Module::Build->new(
	module_name => 'Debian::Build::DependencyTree',
	license     => 'GPL_2',
)->create_build_script();
