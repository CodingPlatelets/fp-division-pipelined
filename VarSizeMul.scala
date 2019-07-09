
// Süleyman Savas, 2016-12-15
// Halmstad University

package cintacc

import chisel3._
 
class VarSizeMul(val size1 : Int, val size2 : Int, val size3 : Int) extends Module {
// tested and works fine

	val io = IO(new Bundle{
		val in1 = Input(UInt(width = size1))
		val in2 = Input(UInt(width = size2))
		val out = Output(UInt(width = size3))
	})

	val result  = Wire(UInt(width = size1 + size2))
	
	result := io.in1 * io.in2
	//io.out := result(size1 + size2 - 3, size1 + size2 - size3-2 )
	io.out := result(size1 + size2 - 1, size1 + size2 - size3 )
	//printf("mult in1: %d , in2: %d out: %d\n", io.in1, io.in2, io.out)
}

class mul2(val size1 : Int, val size2 : Int, val size3 : Int) extends Module {
// tested and works fine

	val io = IO(new Bundle{
		val in1 = Input(UInt(width = size1))
		val in2 = Input(UInt(width = size2))
		val out = Output(UInt(width = size3))
	})

	val result  = Wire(UInt(width = size1 + size2))
	
	result := io.in1 * io.in2
	io.out := result(size1 + size2 - 1, size1 + size2 - size3 )
	//io.out := result(size1 + size2 - 1, size1 + size2 - size3 )
	//printf("mult in1: %d , in2: %d out: %d\n", io.in1, io.in2, io.in1 * io.in2)
}

class mul3(val size1 : Int, val size2 : Int, val size3 : Int) extends Module {
// tested and works fine

	val io = IO(new Bundle{
		val in1 = Input(UInt(width = size1))
		val in2 = Input(UInt(width = size2))
		val out = Output(UInt(width = size3))
	})

	val result  = Wire(UInt(width = size1 + size2))
	
	result := io.in1 * io.in2
	io.out := result(size1 + size2 - 1, size1 + size2 - size3 )
	//io.out := result(size1 + size2 - 1, size1 + size2 - size3 )
	//printf("mult in1: %d , in2: %d out: %d\n", io.in1, io.in2, io.out)
}
