"""This module defines colors used throughout the FIC website."""
from selenium.webdriver.support.color import Color


def hex_to_rgba(hex_string):
    """Convert a hex color into a rgba value for use in tests."""
    return Color.from_string(hex_string).rgba


BLACK = hex_to_rgba('#000000')

GREEN_PRICE = hex_to_rgba('#51c620')
