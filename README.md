FIC Integration Tests
======================

This haskell project contains automated tests to ensure that ic.org behaves as
we expect.

We use HSpec, WebDriver and Selenium to run tests in an actual browser. These
tests are implementation-agnostic and validate only the user experience, not
any specific underlying website code.


Prereqs
--------

You need a selenium server running. It's easiest to have this in a virtual
framebuffer using `Xvfb`, this prevents an actual window from popping up and
disrupting you.

1. Install `xvfb`: `sudo pacman -S xorg-server-xvfb`
1. Install `selenium-server` from the `AUR`: `packer -S selenium-server-standalone`
1. Create a new virtual framebuffer: `sudo Xvfb :20 -ac &`
1. Start Selenium in the new display:
    `DISPLAY=:20 java -jar /usr/share/selenium-server/selenium-server-standalone.jar -timeout=20 -browserTimeout=60`

If you ever **do** want the web browser to pop up on your display, either
remove the `DISPLAY=:20` or change it to `DISPLAY=:0`.

We use `cabal` to create a dependency sandbox and install the tests
dependencies:

1. Install cabal: `sudo pacman -S cabal-install`
1. Update package list: `cabal update`
1. Create a sandbox: `cabal sandbox init`
1. Install dependencies: `cabal install --only-dependencies -j`


Running the Tests
------------------

Use cabal:

    cabal test

You might want some extra flags:

    cabal test -j --show-details=always --test-option=--color

Which we type often enough to alias:

    alias hspec='cabal test -j --show-details=always --test-option=--color'


Building the Documentation
----------------------------

Again, use cabal:

    cabal haddock --tests

Then open up `dist/doc/html/fic-tests/fic/index.html`.


Contribute
-----------

Some Standards:

* Add tests to modules under `Main.Tests`, never directly to `Main.Tests`.
* Add new test categories as sub-modules of the `Main.Tests`.
* Don't hardcode colors into tests, instead put them in `Main.Colors`.
* Abstracted expectations(`should` functions) should be in `Main.Expectations`.


Sample Output
--------------

You get a nice spec that looks like this:

```
ic.org Tests using Chrome
  - (unspecified behavior)

  Home Page Tests
    for the home page
      - opens the home page
      - has the correct title
      - has the correct page heading
      - has a link to the login page

  Error Page Tests
    for 404 pages
      - opens a non-existant page
      - has the correct title
      - has the correct page heading
      - has a search box
      - opens a non-existant page in the /directory/ sub-URI
      - has extra help text about draft listings

...

    for recurring products
      - opens the Product Details page of a recurring product

      the product price
        - is the correct size
     # PENDING: See Bug #359
        - is prefixed by black `From:` text
     # PENDING: See Bug #360

Finished in 55.9434 seconds
33 examples, 0 failures, 7 pending

Test suite fic: PASS
```
