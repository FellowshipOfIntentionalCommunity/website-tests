"""Test Expectations for Events."""
from tests.utils import SeleniumTestCase


class EventDetailsTests(SeleniumTestCase):
    """Test Expectations for the Event Details Page."""
    def setUp(self):
        """Visit an Events Page."""
        self.visit('/events/spirit-jam-drum-circle-2014-09-12/')

    def test_content_is_not_empty(self):
        """It should show the Event's details."""
        contents = self.selenium.find_element_by_css_selector(".entry-content")
        self.assert_in(
            "Join Earth Rhythms Healing for an intentional drum circle",
            contents.text)

    def test_has_google_map(self):
        """It should have a Google Map with a balloon popup."""
        self.assert_element_exists(self.selenium.find_element_by_css_selector,
                                   ".em-map-balloon-content")
