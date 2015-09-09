'''Test Expectations for Community Directory Pages.'''
from tests.utils import SeleniumTestCase


class CommunityListTests(SeleniumTestCase):
    '''Test Expectations for the Community List Pages.'''

    def test_draft_communities_hidden(self):
        '''Draft Communities should not be shown.'''
        self.visit('directory/newest-communities/')
        directory_links = self.selenium.find_elements_by_css_selector(
            'div.cmtylistblock strong a'
        )
        directory_urls = [link.get_attribute('href')
                          for link in directory_links]

        _ = [self.assertNotIn('?post_type=directory&p=', url)
             for url in directory_urls]
