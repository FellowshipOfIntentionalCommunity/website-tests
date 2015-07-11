"""This module defines global settings used in the tests."""
import os

from selenium.webdriver.common.desired_capabilities import DesiredCapabilities


# The base URL to make requests to. Choose between ic.org and
# intcommunity.staging.wpengine.com
BASE_URL = "http://www.ic.org/"
# BASE_URL = "http://intcommunity.staging.wpengine.com/"

# The trailing suffix at the end of every Page Title.
SITE_TITLE_SUFFIXES = [
    " | The Fellowship for Intentional Community website",
    " - Fellowship for Intentional Community"
]

# Local or Travis Selenium Config
if os.environ("TRAVIS"):
    SELENIUM_SERVER = "http://{}:{}@localhost:4445/wd/hub".format(
        os.environ['SAUCE_USERNAME'], os.environ['SAUCE_ACCESS_KEY'])
    SELENIUM_CAPABILITIES = {
        'tunnel-identifier': os.environ['TRAVIS_JOB_NUMBER'],
        'build': os.environ["TRAVIS_BUILD_NUMBER"],
        'tags': [os.environ["TRAVIS_PYTHON_VERSION"], "CI"]
    }
else:
    SELENIUM_SERVER = 'http://127.0.0.1:4444/wd/hub'
    SELENIUM_CAPABILITIES = DesiredCapabilities.CHROME
