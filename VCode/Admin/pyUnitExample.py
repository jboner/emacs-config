#!/usr/bin/env python
#
# This example shows how to write a pyUnit test
import unittest

class SampleTestCase(unittest.TestCase):
    def setUp(self):
        self.var = 100
    def test_always_succeeds(self):
        assert self.var == 100, 'This should NOT be failing'
    def test_always_fail(self):
        assert self.var == 2, "This should always fail"

# Make this test module runnable from the command prompt
if __name__ == "__main__":
    unittest.TextTestRunner().run(unittest.makeSuite(SampleTestCase, 'test'))
