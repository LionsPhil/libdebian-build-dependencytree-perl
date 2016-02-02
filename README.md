# Debian::Build::DependencyTree 
**A Perl library for computing a build order for Debian packages**

This library, given a set of `Dpkg::Control::Info` objects (e.g. loaded from a set of `debian/control` files), will analyze their dependencies and produce an ordering in which they can be built. This is useful for ensuring non-trivial systems made of multiple packages have their individual components built in the correct ordering for bootstrapping, and to represent any changes that ripple down from their dependencies.

Proper CPAN/Debian packaging to follow.
