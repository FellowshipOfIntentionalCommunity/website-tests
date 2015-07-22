"""Test Expectations for Store Pages."""
import pytest
from selenium.common.exceptions import NoSuchElementException

from tests import colors
from tests.utils import SeleniumTestCase


class ProductCategoryTests(SeleniumTestCase):
    """Test Expectations for the Product Category Pages."""
    def setUp(self):
        """Visit a product category page with a suggested price."""
        self.visit("community-bookstore/category/donations-to-fic/")

    def test_suggested_prices_are_hidden(self):
        """Products with suggested prices should have their prices hidden."""
        suggested_price_products = self.selenium.find_elements_by_css_selector(
            "li.nyp-product")
        for product in suggested_price_products:
            self.assert_element_doesnt_exist(
                product.find_element_by_css_selector, "span.price")

    def test_result_count_is_hidden(self):
        """The number of results should be hidden."""
        result_count = self.selenium.find_element_by_css_selector(
            '.tax-product_cat.woocommerce #content .woocommerce-result-count')
        self.assert_css_property_equals(result_count, 'display', 'none')


class ProductDetailTests(SeleniumTestCase):
    """Test Expectations for the Product Details Pages."""
    def setUp(self):
        """Visit a product's details page."""
        self.visit("/community-bookstore/product/gaia-education-guides/")

    @pytest.mark.xfail
    def test_price_is_correct_size(self):
        """The price should be the correct size."""
        _assert_price_is_correct_size(self)

    def test_price_is_green(self):
        """The price should be green."""
        _assert_price_is_green(self, "p.price")

    def test_price_is_not_prefixed_or_suffixed(self):
        """The price should not be prefixed or suffixed by any text."""
        price = self.selenium.find_element_by_css_selector("p.price")
        amount = self.selenium.find_element_by_css_selector(
            "p.price span.amount")
        self.assert_equal(price.text, amount.text)


class ProductSearchTests(SeleniumTestCase):
    """Test Expectations for the Store Product Search."""

    def setUp(self):
        """Search for a product containing the word 'consensus'."""
        self.visit('/?s=consensus&post_type=product')

    def test_page_description_is_hidden(self):
        """The Page's description should be hidden."""
        description = self.selenium.find_element_by_css_selector(
            'body.search-results.woocommerce #content .page-description')
        self.assert_css_property_equals(description, 'display', 'none')


class SuggestedPriceProductDetailTests(SeleniumTestCase):
    """Test Expectations for Suggested Price Product Detail Pages."""
    def setUp(self):
        """Visit the details page of a produt with a suggested price."""
        self.visit("/community-bookstore/product/website-donation/")

    def test_price_is_empty(self):
        """The price should not be shown."""
        price = self.selenium.find_element_by_css_selector(
            "div.summary div p.price")
        self.assert_equal("", price.text)

    @pytest.mark.xfail
    def test_price_input_has_placeholder(self):
        """The price input should have a placeholder. See Bug #358"""

    @pytest.mark.xfail
    def test_price_input_has_no_text(self):
        """The price input should not have any entered text. See Bug #358"""


class RecurringProductDetailTests(SeleniumTestCase):
    """Test Expectations for the Details Pages of Recurring Products."""
    def setUp(self):
        """Visit the details page of a recuring product."""
        self.visit("/community-bookstore/product/subscription/")

    @pytest.mark.xfail
    def test_price_is_correct_size(self):
        """The price should be the correct size."""
        _assert_price_is_correct_size(self)

    def test_price_is_green(self):
        """The price should be green."""
        _assert_price_is_green(self, "p.price")

    @pytest.mark.xfail
    def test_price_is_prefixed_by_from_text(self):
        """The price should be prefixed by black `From:` text."""
        from_element = self.selenium.find_element_by_css_selector(
            "p.price span.from")
        self.assert_equal("From:", from_element.text)
        self.assert_css_property_equals(from_element, "color", colors.BLACK)

    @pytest.mark.xfail
    def test_price_is_suffixed_by_year_text(self):
        """The price should be suffixed by black `/ year` text."""
        year_element = self.selenium.find_element_by_css_selector(
            "p.price")
        self.assert_in(" / year", year_element.text)
        self.assert_css_property_equals(year_element, "color", colors.BLACK)


def _assert_price_is_correct_size(obj):
    """Assert that the price and the surrounding text have a size of 14px."""
    price = obj.selenium.find_element_by_css_selector(".price")
    obj.assert_css_property_equals(price, "font-size", "14px")

    amount = obj.selenium.find_element_by_css_selector(".amount")
    obj.assert_css_property_equals(amount, "font-size", "14px")


def _assert_price_is_green(obj, parent):
    """Assert that the price, under an optional parent, is green."""
    try:
        element = obj.selenium.find_element_by_css_selector(
            "{} ins span.amount".format(parent))
    except NoSuchElementException:
        element = obj.selenium.find_element_by_css_selector(
            "{} span.amount".format(parent))
    obj.assert_css_property_equals(element, "color", colors.GREEN_PRICE)
