import TestCaseWithHelpers

class DeliberateOutputTest(TestCaseWithHelpers.TestCaseWithHelpers):

    ##############################################################
    #
    # test below should produce errors, when run with mediator test
    # of voicecode they should give the predicted results in the
    # output file (foo or foo.txt or correct.results.emacs.dat)
    #
    ##############################################################

    def test_show_deliberate_output_assert_equal(self):
        self.assert_equal("assert_", "_equal", "this test should produce output in foo.txt")
        self.fail("should not come here")

    # these are unittest test functions:
    def test_show_deliberate_output_assertEqual(self):
        self.assertEqual("assert", "Equal", "this test should produce output in foo.txt")
        self.fail("should not come here")

    def test_show_deliberate_output_assert_(self):
        self.assert_("assert_" == "foo", "this test should produce output in foo.txt")
        self.fail("should not come here")

    def test_show_deliberate_output_assertRaises(self):
        self.assertRaises(ValueError, "does not" + "raise a ValueError")
        self.fail("should not come here")

    def test_show_deliberate_output_assertRaises(self):
        self.assertRaises(KeyError, "Key" + 3)
        self.fail("should not come here")
        
    def test_show_deliberate_output_always_fail(self):
        self.fail("always fail, should produce output in foo.txt")
        self.fail("should not come here")
        

