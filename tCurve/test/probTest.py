from unittest import TestCase
from tCurve.prob import prob as prob
from tCurve.prob import _integrate
from tCurve.prob import _f
from tCurve.prob import _calculateConstant
import json


class ProbTest(TestCase):

    def setUp(self):
        self.nominalN = 4
        self.nominalT = 1.4398
        self.nominalTails = 1
        self.inputDictionary = {}
        self.errorValue = "error:"
        self.errorKey = "error"
        self.solutionKey = "probability"
        
    def tearDown(self):
        self.inputDictionary = {}

    def setT(self, t):
        self.inputDictionary["t"] = t

    def setN(self, n):
        self.inputDictionary["n"] = n

    def setTails(self, tails):
        self.inputDictionary["tails"] = tails
        
    def setExtra(self, extra):
        self.inputDictionary["extra"] = extra

    # 100 prob
    #    Desired level of confidence:    boundary value analysis
    #    Input-output Analysis
    #        inputs:        n -> integer, .GE.3, mandatory, unvalidated
    #                       t ->    float > 0.0, mandatory, unvalidated
    #                       tails -> integer, 1 or 2, optional, defaults to 1
    #        outputs:    float .GT. 0 .LE. 1.0
    #    Happy path analysis:
    #       n:       nominal value    n=6
    #                low bound        n=3
    #        t:      nominal value    t=1.4398
    #                low bound        t>0.0
    #        tails:  value 1          tails = 1
    #                value 2          tails = 2
    #                missing tails
    #        output:
    #                The output is an interaction of t x tails x n:
    #                    nominal t, 1 tail
    #                    nominal t, 2 tails
    #                    low n, low t, 1 tail
    #                    low n, low t, 2 tails
    #                    high n, low t, 1 tail
    #                    high n, low t, 2 tails
    #                    low n, high t, 1 tail
    #                    low n, high t, 2 tails
    #                    high n, high t, 1 tail
    #                    high n, high t, 2 tails
    #                    nominal t, default tails
    #    Sad path analysis:
    #        n:      missing n
    #                out-of-bound n   n<3
    #                non-integer n    n = 2.5
    #        t:      missing t
    #                out-of-bounds n  t<0.0
    #                non-numeric t    t="abc"
    #        tails:  invalid tails    tails = 3
    #
    # Happy path
    def test100_010ShouldCalculateNominalCase1TailHttp(self):
        self.setT(1.8946)
        self.setN(7)
        self.setTails(1)
        self.setExtra("a")
        result = prob(self.inputDictionary)
        self.assertAlmostEqual(result[self.solutionKey], 0.950, 3)
     
    def test100_010ShouldCalculateNominalCase1Tail(self):
        self.setT(1.8946)
        self.setN(7)
        self.setTails(1)
        result = prob(self.inputDictionary)
        self.assertAlmostEqual(result[self.solutionKey], 0.950, 3)
  
    def test100_020ShouldCalculateNominalCase2Tail(self):
        self.setT(1.8946)
        self.setN(7)
        self.setTails(2)
        result = prob(self.inputDictionary)
        self.assertAlmostEqual(result[self.solutionKey], 0.900, 3)
  
    def test100_030ShouldCalculateLowNLowT1TailEdgeCase(self):
        self.setT(0.2767)
        self.setN(3)
        self.setTails(1)
        result = prob(self.inputDictionary)
        self.assertAlmostEqual(result[self.solutionKey], 0.600, 3)
  
    def test100_040ShouldCalculateLowNLowT2TailEdgeCase(self):
        self.setT(0.2767)
        self.setN(3)
        self.setTails(2)
        result = prob(self.inputDictionary)
        self.assertAlmostEqual(result[self.solutionKey], 0.200, 3)
  
    def test100_050ShouldCalculateHighNLowT1TailEdgeCase(self):
        self.setT(0.2567)
        self.setN(20)
        self.setTails(1)
        result = prob(self.inputDictionary)
        self.assertAlmostEqual(result[self.solutionKey], 0.600, 3)
  
    def test100_060ShouldCalculateHighNLowT2TailEdgeCase(self):
        self.setT(0.2567)
        self.setN(20)
        self.setTails(2)
        result = prob(self.inputDictionary)
        self.assertAlmostEqual(result[self.solutionKey], 0.200, 3)
  
    def test100_070ShouldCalculateLowNHighT1EdgeCase(self):
        self.setT(5.8409)
        self.setN(3)
        self.setTails(1)
        result = prob(self.inputDictionary)
        self.assertAlmostEqual(result[self.solutionKey], 0.995, 3)
  
    def test100_080ShouldCalculateLowNHighT2EdgeCase(self):
        self.setT(5.8409)
        self.setN(3)
        self.setTails(2)
        result = prob(self.inputDictionary)
        self.assertAlmostEqual(result[self.solutionKey], 0.990, 3)
  
    def test100_090ShouldCalculateHighHighT1TailEdgeCase(self):
        self.setT(2.8453)
        self.setN(20)
        self.setTails(1)
        result = prob(self.inputDictionary)
        self.assertAlmostEqual(result[self.solutionKey], 0.995, 3)
  
    def test100_100ShouldCalculateHighHighT2TailEdgeCase(self):
        self.setT(2.8453)
        self.setN(20)
        self.setTails(2)
        result = prob(self.inputDictionary)
        self.assertAlmostEqual(result[self.solutionKey], 0.990, 3)
  
    def test100_110ShouldCalculateWithDefaultTails(self):
        self.setT(1.8946)
        self.setN(7)
        result = prob(self.inputDictionary)
        self.assertAlmostEqual(result[self.solutionKey], 0.900, 3)
  
    # Sad path
    def test100_910ShouldRaiseExceptionOnMissingT(self):
        self.setN(self.nominalN)
        self.setTails(self.nominalTails)
        result = prob(self.inputDictionary)
        self.assertIn(self.errorKey, result)
        self.assertIn(self.errorValue, result[self.errorKey])
  
    def test100_920ShouldRaiseExceptionOnOutOfBoundsT(self):
        self.setT(-1.0)
        self.setN(self.nominalN)
        self.setTails(self.nominalTails)
        result = prob(self.inputDictionary)
        self.assertIn(self.errorKey, result)
        self.assertIn(self.errorValue, result[self.errorKey])
  
    def test100_930ShouldRaiseExceptionOnNonNumericT(self):
        self.setT("abc")
        self.setN(self.nominalN)
        self.setTails(self.nominalTails)
        result = prob(self.inputDictionary)
        self.assertIn(self.errorKey, result)
        self.assertIn(self.errorValue, result[self.errorKey])
  
    def test100_940ShouldRaiseExceptionOnInvalidTails(self):
        self.setTails(0)
        self.setT(self.nominalT)
        self.setN(self.nominalN)
        result = prob(self.inputDictionary)
        self.assertIn(self.errorKey, result)
        self.assertIn(self.errorValue, result[self.errorKey])
  
    def test100_950ShouldRaiseExceptionOnMissingN(self):
        self.setT(self.nominalT)
        self.setTails(1)
        result = prob(self.inputDictionary)
        self.assertIn(self.errorKey, result)
        self.assertIn(self.errorValue, result[self.errorKey])
  
    def test100_960ShouldRaiseExceptionOnOutOfBoundN(self):
        self.setN(0)
        self.setT(self.nominalT)
        self.setTails(1)
        result = prob(self.inputDictionary)
        self.assertIn(self.errorKey, result)
        self.assertIn(self.errorValue, result[self.errorKey])
  
    def test100_970ShouldRaiseExceptionOnNonIntegerN(self):
        self.setN(2.5)
        self.setT(self.nominalT)
        self.setTails(1)
        result = prob(self.inputDictionary)
        self.assertIn(self.errorKey, result)
        self.assertIn(self.errorValue, result[self.errorKey])
        
#   200 _integrate
#    Desired level of confidence: BVA
#    input-output analysis:
#    inputs:
#        n  -> integer, .GE.3, mandatory, validated
#        t  ->    float > 0.0, mandatory, validated
#        _f -> function, already constructed and tested
#    outputs:
#        float .GE. 0 .LE. 1.0
#    
#    Happy path analysis:
#        test 010: t = 1.8946, n = 7, tail = 1, output = 0.950
#        test 020: t = 1.8946, n = 7, tail = 2, output = 0.900
#        test 030: t = 0.2767, n = 3, tail = 1, output = 0.600
#        test 040: t = 0.2767, n = 3, tail = 2, output = 0.200
#        test 050: t = 5.8409, n = 3, tail = 1, output = 0.995
#        test 060: t = 5.8409, n = 3, tail = 2, output = 0.990
#        test 070: t = 2.8453, n = 20, tail = 1, output = 0.995
#        test 080: t = 2.8453, n = 20, tail = 2, output = 0.990
#        test 090: t = 0.2602, n = 10, tail = 1, output = 0.600
#        test 100: t = 0.2602, n = 10, tail = 2, output = 0.200
#        test 110: t = 0.5329, n = 20, tail = 1, output = 0.700
#        test 120: t = 0.5329, n = 20, tail = 2, output = 0.400
#        test 120: t = 1.0997, n = 9, tail = 1, output = 0.850
#        test 140: t = 1.0997, n = 9, tail = 2, output = 0.700
#        test 150: t = 1.3104, n = 30, tail = 1, output = 0.900
#        test 160: t = 1.3104, n = 30, tail = 2, output = 0.800
#        test 170: t = 2.0150, n = 5, tail = 1, output = 0.950
#        test 180: t = 2.0150, n = 5, tail = 2, output = 0.900
#        test 190: t = 2.9980, n = 7, tail = 1, output = 0.990
#        test 200: t = 2.9980, n = 7, tail = 2, output = 0.980
#    sad path analysis:
#        all the inputs are validated and bounded. No sad path.


#Happy path
    def test200_010_OneTail(self):
        t = 1.8946
        n = 7
        constant = _calculateConstant(n)
        integration = _integrate(t, n, _f)
        result = constant * integration + 0.5
        self.assertAlmostEqual(result, 0.950, 3)

    def test200_020_TwoTails(self):
        t = 1.8946
        n = 7
        constant = _calculateConstant(n)
        integration = _integrate(t, n, _f)
        result = constant * integration * 2
        self.assertAlmostEqual(result, 0.900, 3)
        
    def test200_030_OneoTail(self):
        t = 0.2767
        n = 3
        constant = _calculateConstant(n)
        integration = _integrate(t, n, _f)
        result = constant * integration + 0.5
        self.assertAlmostEqual(result, 0.600, 3)
        
    def test200_040_TwoTails(self):
        t = 0.2767
        n = 3
        constant = _calculateConstant(n)
        integration = _integrate(t, n, _f)
        result = constant * integration * 2
        self.assertAlmostEqual(result, 0.200, 3)   

    def test200_050_OneTail(self):
        t = 5.8409
        n = 3
        constant = _calculateConstant(n)
        integration = _integrate(t, n, _f)
        result = constant * integration + 0.5
        self.assertAlmostEqual(result, 0.995, 3)
        
    def test200_060_TwoTails(self):
        t = 5.8409
        n = 3
        constant = _calculateConstant(n)
        integration = _integrate(t, n, _f)
        result = constant * integration * 2
        self.assertAlmostEqual(result, 0.990, 3)

    def test200_070_OneTail(self):
        t = 2.8453
        n = 20
        constant = _calculateConstant(n)
        integration = _integrate(t, n, _f)
        result = constant * integration + 0.5
        self.assertAlmostEqual(result, 0.995, 3)
        
    def test200_080_TwoTails(self):
        t = 2.8453
        n = 20
        constant = _calculateConstant(n)
        integration = _integrate(t, n, _f)
        result = constant * integration * 2
        self.assertAlmostEqual(result, 0.990, 3)
        
    def test200_090_OneTail(self):
        t = 0.2602
        n = 10
        constant = _calculateConstant(n)
        integration = _integrate(t, n, _f)
        result = constant * integration + 0.5
        self.assertAlmostEqual(result, 0.600, 3)
        
    def test200_100_TwoTails(self):
        t = 0.2602
        n = 10
        constant = _calculateConstant(n)
        integration = _integrate(t, n, _f)
        result = constant * integration * 2
        self.assertAlmostEqual(result, 0.200, 3)
        
    def test200_110_OneTail(self):
        t = 0.5329
        n = 20
        constant = _calculateConstant(n)
        integration = _integrate(t, n, _f)
        result = constant * integration + 0.5
        self.assertAlmostEqual(result, 0.700, 3)
        
    def test200_120_TwoTails(self):
        t = 0.5329
        n = 20
        constant = _calculateConstant(n)
        integration = _integrate(t, n, _f)
        result = constant * integration * 2
        self.assertAlmostEqual(result, 0.400, 3)
        
    def test200_130_OneTail(self):
        t = 1.0997
        n = 9
        constant = _calculateConstant(n)
        integration = _integrate(t, n, _f)
        result = constant * integration + 0.5
        self.assertAlmostEqual(result, 0.850, 3)
        
    def test200_140_TwoTails(self):
        t = 1.0997
        n = 9
        constant = _calculateConstant(n)
        integration = _integrate(t, n, _f)
        result = constant * integration * 2
        self.assertAlmostEqual(result, 0.700, 3)
        
    def test200_150_OneTail(self):
        t = 1.3104
        n = 30
        constant = _calculateConstant(n)
        integration = _integrate(t, n, _f)
        result = constant * integration + 0.5
        self.assertAlmostEqual(result, 0.900, 3)
        
    def test200_160_TwoTails(self):
        t = 1.3104
        n = 30
        constant = _calculateConstant(n)
        integration = _integrate(t, n, _f)
        result = constant * integration * 2
        self.assertAlmostEqual(result, 0.800, 3)

    def test200_170_OneTail(self):
        t = 2.0150
        n = 5
        constant = _calculateConstant(n)
        integration = _integrate(t, n, _f)
        result = constant * integration + 0.5
        self.assertAlmostEqual(result, 0.950, 3)
        
    def test200_180_TwoTails(self):
        t = 2.0150
        n = 5
        constant = _calculateConstant(n)
        integration = _integrate(t, n, _f)
        result = constant * integration * 2
        self.assertAlmostEqual(result, 0.900, 3)
        
    def test200_190_OneTail(self):
        t = 2.9980
        n = 7
        constant = _calculateConstant(n)
        integration = _integrate(t, n, _f)
        result = constant * integration + 0.5
        self.assertAlmostEqual(result, 0.990, 3)
        
    def test200_200_TwoTails(self):
        t = 2.9980
        n = 7
        constant = _calculateConstant(n)
        integration = _integrate(t, n, _f)
        result = constant * integration * 2
        self.assertAlmostEqual(result, 0.980, 3)





   
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
