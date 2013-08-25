# -*- coding: utf-8 -*-

# $Id: $


import unittest

class Test(>>>TEST_NAME<<<)(unittest.TestCase):

    def setUp(self):
        pass

    def test_(>>>POINT<<<)something(self):
        pass


def test_suite() :
    suite = unittest.makeSuite(Test(>>>TEST_NAME<<<))
    # suite.addTest(unittest.makeSuite(AnotherTest))
    return suite

def main() :
    unittest.TextTestRunner(verbosity=0).run(test_suite())

if __name__ == '__main__':
    main()

>>>TEMPLATE-DEFINITION-SECTION<<<
("TEST_NAME" "Test name: " "" "" "SomeTest")
