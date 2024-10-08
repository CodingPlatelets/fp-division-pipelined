@Author Suleyman Savas
contact: suleyman_savas@hotmail.com

The origin author is above, and now this version is authored by @Platelets
contact: wenkanedward@gmail.com

Floating point division written in Chisel - using harmonized parabolic synthesis. The implementation has 6 pipeline stages. When continuously fed, the throughput is 1 result/cycle.

The divisor is inverted and the result is multiplied by the dividend.

Internal format is not IEEE 754 single precision float. (Mantissas are the same but exponents are different.)

Most significant 6 bits of the mantissa are used for addressing the coefficients.

Conversion of coefficients into binary (coeffcients are smaller than 1):
decimal 0.25 = binary 0.010
decimal 0.625 = binary 0.101 (integer part and fraction part are converted seperately)

Implementation details, results and discussions can be found in the following article:

"Efficient Single-Precision Floating-Point Division Using Harmonized Parabolic Synthesis"
https://ieeexplore.ieee.org/abstract/document/7987504/


How to compile:

Go into the folder

To run the tests and generate harness: sbt "run --backend c --compile --test --genHarness"

Generating verilog: sbt "run --backend v --genHarness"

More info on Chisel: https://www.chisel-lang.org/
