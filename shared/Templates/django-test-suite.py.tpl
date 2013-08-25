# -*- coding: utf-8 -*-

# $Id: $


from django.contrib.auth.models import User
from django.test import TestCase
from django.test.client import RequestFactory
from django.utils import unittest

# from app import models as app_models


class Test(>>>TEST_NAME<<<)(TestCase):

    def setUp(self):
        self.user = User.objects.create_superuser("test", "", password="123456")
        self.client.login(username="test", password="123456")

    def test_(>>>POINT<<<)something(self):
        pass


def suite() :
    suite = unittest.makeSuite(Test(>>>TEST_NAME<<<))
    # suite.addTest(unittest.makeSuite(AnotherTest))
    return suite

def main() :
    unittest.TextTestRunner(verbosity=0).run(suite())

if __name__ == '__main__':
    main()

>>>TEMPLATE-DEFINITION-SECTION<<<
("TEST_NAME" "Test name: " "" "" "SomeTest")
