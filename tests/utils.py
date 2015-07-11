"""This module contains utility functions and classes used in the package."""
import unittest

from selenium import webdriver
from selenium.common.exceptions import NoSuchElementException

from tests import settings


class SeleniumTestCase(unittest.TestCase):
    """A test case with a WebDriver and helper functions."""
    @classmethod
    def setup_class(cls):
        """Create a new Selneium Remote WebDriver Connection.

        The Selenium server is expected to be running at
        http://127.0.0.1:4444/wd/hub and the Chrome WebDriver should be
        available.

        """
        cls.selenium = webdriver.Remote(
            command_executor=settings.SELENIUM_SERVER,
            desired_capabilities=settings.SELENIUM_CAPABILITIES)
        cls.selenium.implicitly_wait(15)
        super(SeleniumTestCase, cls).setup_class()

    @classmethod
    def teardown_class(cls):
        """Close the Selenium Web Browser."""
        cls.selenium.quit()

    def visit(self, relative_url):
        """Append the base domain and visit the URL."""
        self.selenium.get(settings.BASE_URL + relative_url)

    def assert_title_equals(self, expected_title):
        """Assert the current page title is correct.

        The trailing SITE_TITLE_SUFFIX is automatically handled.

        """
        full_titles = [expected_title + suffix for suffix in
                       settings.SITE_TITLE_SUFFIXES]
        self.assert_in(self.selenium.title,
                       full_titles,
                       msg="The actual page title was '{}' instead of '{}'".
                       format(self.selenium.title, full_titles[0]))

    def assert_page_heading_equals(self, expected_heading):
        """Assert the main heading on the current page is correct."""
        heading = self.selenium.find_element_by_css_selector("h1")
        self.assert_equal(expected_heading,
                          heading.text,
                          "The actual page heading was '{}' instead of '{}'".
                          format(heading.text, expected_heading))

    def assert_attribute_equals(self, element, attribute, expected_value):
        """Assert an element has the expected attribute value."""
        actual_attribute = element.get_attribute(attribute)
        self.assert_equal(actual_attribute,
                          expected_value,
                          "The actual attribute was '{}' instead of '{}'".
                          format(actual_attribute, expected_value))

    def assert_css_property_equals(self, element, css_prop, expected_value):
        """Assert an element has the expected CSS property value."""
        actual_value = element.value_of_css_property(css_prop)
        self.assert_equal(actual_value,
                          expected_value,
                          "The actual CSS {} was '{}' instead of '{}'".
                          format(css_prop, actual_value, expected_value))

    def assert_element_exists(self, selector_function, *args, **kwargs):
        """Assert an element can be found using the selector and arguments."""
        try:
            selector_function(*args, **kwargs)
        except NoSuchElementException:
            self.fail("Could not find the element defined with:\n{}\n{}".
                      format(args, kwargs))

    def assert_element_doesnt_exist(self, selector_function, *args, **kwargs):
        self.assert_raises(
            NoSuchElementException, selector_function, *args, **kwargs)

    def assert_equal(self, *args, **kwargs):
        """Wrap the assertEqual function."""
        self.assertEqual(*args, **kwargs)

    def assert_in(self, *args, **kwargs):
        """Wrap the assertIn function."""
        self.assertIn(*args, **kwargs)

    def assert_not_in(self, *args, **kwargs):
        """Wrap the assertNotIn function."""
        self.assertNotIn(*args, **kwargs)

    def assert_raises(self, *args, **kwargs):
        """Wrap the assertRaises function."""
        self.assertRaises(*args, **kwargs)
