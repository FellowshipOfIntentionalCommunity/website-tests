"""Test Expectations for Store Pages."""
from nose.plugins.skip import SkipTest
from selenium.common.exceptions import NoSuchElementException

from tests import colors
from tests.utils import SeleniumTestCase


class TopRatedProductsWidgetTests(SeleniumTestCase):
    """Test Expectations for the Top Rated Products Widget."""
    def setUp(self):
        """Visit the Home Page."""
        self.visit('/')

    def test_price_is_correct_size(self):
        """The price should be the same size as the surrounding text."""
        _assert_price_is_correct_size(self)

    def test_price_is_prefixed_by_from_text(self):
        """The price should be prefixed by black `From:` text if recurring."""
        reccurring_elements = self.selenium.find_elements_by_css_selector(
            "ul.product_list_widget li span.from")
        for element in reccurring_elements:
            self.assert_equal("From:", element.text)
            self.assert_css_property_equals(element, "color", colors.BLACK)

    def test_price_is_green(self):
        """The price should be green."""
        _assert_price_is_green(self, "li")

    def test_price_is_suffixed_by_year_text(self):
        """The price should by suffixed by black `/ year` text if recurring."""
        products = self.selenium.find_elements_by_css_selector(
            "ul.product_list_widget li")
        for product in products:
            has_recurring_text = product.find_elements_by_css_selector(
                "span.from")
            if has_recurring_text:
                self.assert_in("/ year", product.text)
                self.assert_css_property_equals(product, "color", colors.BLACK)


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


class ProductDetailTests(SeleniumTestCase):
    """Test Expectations for the Product Details Pages."""
    def setUp(self):
        """Visit a product's details page."""
        self.visit("/community-bookstore/product/wemoon-datebook/")

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

    def test_price_input_has_placeholder(self):
        """The price input should have a placeholder."""
        raise SkipTest('See Bug #358')

    def test_price_input_has_no_text(self):
        """The price input should not have any entered text."""
        raise SkipTest('See Bug #358')


class RecurringProductDetailTests(SeleniumTestCase):
    """Test Expectations for the Details Pages of Recurring Products."""
    def setUp(self):
        """Visit the details page of a recuring product."""
        self.visit("/community-bookstore/product/subscription/")

    def test_price_is_correct_size(self):
        """The price should be the correct size."""
        _assert_price_is_correct_size(self)

    def test_price_is_green(self):
        """The price should be green."""
        _assert_price_is_green(self, "p.price")

    def test_price_is_prefixed_by_from_text(self):
        """The price should be prefixed by black `From:` text."""
        raise SkipTest('See Bug #360')
        from_element = self.selenium.find_element_by_css_selector(
            "p.price span.from")
        self.assert_equal("From:", from_element.text)
        self.assert_css_property_equals(from_element, "color", colors.BLACK)

    def test_price_is_suffixed_by_year_text(self):
        """The price should be suffixed by black `/ year` text."""
        raise SkipTest('See Bug #360')
        year_element = self.selenium.find_element_by_css_selector(
            "p.price")
        self.assert_in(" / year", year_element.text)
        self.assert_css_property_equals(year_element, "color", colors.BLACK)


def _assert_price_is_correct_size(obj):
    """Assert that the price and the surrounding text have a size of 14px."""
    raise SkipTest('See Bug #359')
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
