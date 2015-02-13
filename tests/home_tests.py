"""Tests for the Home Page."""
from tests import settings
from tests.utils import SeleniumTestCase


class HomePageTests(SeleniumTestCase):
    """Test Expectations for the Home Page."""
    def setUp(self):
        """Visit the Home Page."""
        self.visit("/")

    def test_has_correct_title(self):
        """It should have the correct page title."""
        self.assert_title_equals("Welcome to FIC")

    def test_has_correct_page_heading(self):
        """It should have the correct page heading."""
        self.assert_page_heading_equals(
            "Welcome to the Fellowship for Intentional Community")

    def test_has_link_to_login(self):
        """It should have a link to the login page."""
        link_element = self.selenium.find_element_by_link_text("Login")
        self.assert_attribute_equals(
            link_element, "href", settings.BASE_URL + "wp-login.php")
