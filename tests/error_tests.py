"""Test the various HTTP Error pages(404, 502, etc.)."""
from tests.utils import SeleniumTestCase


class General404Tests(SeleniumTestCase):
    """Test General Expectations for the Custom 404 Page."""
    def setUp(self):
        """Visit a non-existant page."""
        self.visit('/i-am-so/long-that/I-probably/dont-exist')

    def test_has_correct_title(self):
        """It should have the correct page title."""
        self.assert_title_equals("Page Not Found")

    def test_has_correct_page_heading(self):
        """It should have the correct page heading."""
        self.assert_page_heading_equals("Whoops! Page Not Found")

    def test_has_search_box(self):
        """It should have a search box."""
        self.assert_element_exists(
            self.selenium.find_element_by_css_selector, "form#searchform")


class Directory404Tests(SeleniumTestCase):
    """Test Expectations for Directory 404 Pages."""
    def setUp(self):
        """Visit a non-existant page in the /directory/ sub-URI."""
        self.visit("/directory/I-should-not/ever/exist/or-stuffs/screwed")

    def test_has_draft_directory_text(self):
        """It should have extra help text about draft directory listings."""
        content = self.selenium.find_element_by_css_selector(".entry-content")
        self.assert_in("If you are looking for a Community in the Directory",
                       content.text)
