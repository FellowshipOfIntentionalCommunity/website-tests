FIC Integration Tests
======================

[![Build Status](https://travis-ci.org/FellowshipofIntentionalCommunity/website-tests.svg?branch=master)](https://travis-ci.org/FellowshipofIntentionalCommunity/website-tests)

This python project contains automated tests to verify our expectations of the
http://ic.org website.

We use Nose and Selenium to run tests in an actual browser. These tests are
implementation-agnostic and validate only the user experience, not any specific
underlying website code.


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

You will also need `git`, `python` and `pip`. `virtualenv` and
`virtualenvwrapper` are recommended for dependency management:

    sudo pacman -S git python python-pip
    sudo pip install virtualenv virtualenvwrapper

Create a new virtual environment. If you set up `virtualenvwrapper`, it's as
easy as:

    mkvirtualenv fic-tests

Next clone the `git` repository:

    git clone http://bugs.sleepanarchy.com/fic/fic-tests.git

Change into the new directory and install the python dependencies:

    cd fic-tests
    pip install -r requirements.txt


Running the Tests
------------------

Just use `py.test`:

    cd fic-tests
    pip install -r requirements.txt
    py.test

You can run groups of tests by specifiying a filepath or expression to match:

    py.test tests/store_tests.py
    py.test -k ProductDetail
    py.test -k ProductDetail not Suggested
    py.test -k price

Add the `--spec` arguement to more details while running:

    py.test --spec

You can monitor the test files & re-run the tests when they change by using the
`ptw` command.
