FIC Integration Tests
======================

This haskell project contains automated tests to ensure that ic.org behaves as
we expect.

Prereqs
--------

You need a selenium server running. It's easiest to have this in a virtual
framebuffer using Xvfb, this prevents an actual window from popping up and
disrupting you.

1. Install `xvfb`: `sudo pacman -S xorg-server-xvfb`
1. Install `selenium-server` from the `AUR`: `packer -S selenium-server-standalone`
1. Create a new virtual framebuffer: `sudo Xvfb :20 -ac &`
1. Start Selenium in the new display:
    `DISPLAY=:20 java -jar /usr/share/selenium-server/selenium-server-standalone.jar`

Install
--------

1. Install cabal: `sudo pacman -S cabal-install`
1. Update package list: `cabal update`
1. Create a sandbox: `cabal sandbox init`
1. Install dependencies: `cabal install --only-dependencies`
1. Build the test executable: `cabal build -j`
1. Run the test suite:`dist/buil/fic-tests/fic-tests`

Contribute
-----------

Add new test categories as sub-modules of the `Main.Tests`.
