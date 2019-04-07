class MachineState {
    val programMemory = ShortArray(256) // PM
    val microMemory = IntArray(128)     // MyM
    val k1 = ByteArray(16)              // K1
    val k2 = ByteArray(4)               // K2
    var registers = Registers()
    var flag_Z: Boolean = false
    var flag_N: Boolean = false
    var flag_O: Boolean = false
    var flag_C: Boolean = false
    var flag_L: Boolean = false
    var cycleCount: Long = 0
    var halted: Boolean = false


    fun step(){
        if(halted) return
        ++cycleCount

        val instr       = microMemory[registers.uPC.toIndexValue()]
        val instr_ALU   = instr ushr 21 onlyBits 4
        val instr_TB    = instr ushr 18 onlyBits 3
        val instr_FB    = instr ushr 15 onlyBits 3
        val instr_S     = instr.getBitAt(14)
        val instr_P     = instr.getBitAt(13)
        val instr_LC    = instr ushr 11 onlyBits 2
        val instr_SEQ   = instr ushr 7 onlyBits 4
        val instr_ADR   = instr onlyBits 7

        val pInstr      = Instruction.parse(registers.ir)

        val mux_select  = if(instr_S) pInstr.M else pInstr.GRx

        val gr          = when(mux_select.toInt()){
            0 -> registers.gr0
            1 -> registers.gr1
            2 -> registers.gr2
            else -> registers.gr3
        }

        val newRegs     = registers.copy()

        val bus_to      = when(instr_TB){
            0 -> 0xFFFF.toShort()
            1 -> registers.ir
            2 -> programMemory[registers.asr.toIndexValue()]
            3 -> registers.pc.toMaskedShort()
            4 -> registers.ar
            5 -> registers.hr
            6 -> gr
            else -> instr.onlyBits(16).toShort()   // Special TB-mode where uADR is copied to bus
        }

        // Do non-ALU things here
        if(instr_TB != 7){
            when(instr_FB){
                1 -> newRegs.ir = bus_to
                2 -> programMemory[registers.asr.toIndexValue()] = bus_to
                3 -> newRegs.pc = bus_to.toProperType(newRegs.pc)
                5 -> newRegs.hr = bus_to
                6 -> when(mux_select.toInt()){
                    0 -> newRegs.gr0 = bus_to
                    1 -> newRegs.gr1 = bus_to
                    2 -> newRegs.gr2 = bus_to
                    else -> newRegs.gr3 = bus_to
                }
                else -> newRegs.asr = bus_to.toProperType(newRegs.asr)
            }

            if(instr_P) newRegs.pc = (registers.pc + 1).toByte()

            when(instr_LC){
                1 -> newRegs.lc = (registers.lc + 1).toByte()
                2 -> newRegs.lc = bus_to.onlyBits(8).toByte()
                3 -> newRegs.lc = instr.onlyBits(8).toByte() 
            }

            fun jumpIf(cond: Boolean) =
                if(cond) instr_ADR.toByte()
                else (registers.uPC + 1).toByte()

            newRegs.uPC = when(instr_SEQ){
                0 -> (registers.uPC + 1).toByte()
                1 -> k1[pInstr.OP.toIndexValue()]
                2 -> k2[pInstr.M.toIndexValue()]
                3 -> 0.toByte()
                4 -> jumpIf(!flag_Z)
                5 -> instr_ADR.toByte()
                6 -> {
                    newRegs.uSP = registers.uPC
                    instr_ADR.toByte()
                }
                7 -> registers.uSP
                8 -> jumpIf(flag_Z)
                9 -> jumpIf(flag_N)
                10 -> jumpIf(flag_C)
                11 -> jumpIf(flag_O)
                12 -> jumpIf(flag_L)
                13 -> jumpIf(!flag_C)
                14 -> jumpIf(!flag_O)
                else -> {
                    halted = true
                    0.toByte()
                }
            }
        }

        newRegs.ar = when(instr_ALU){
            0 -> registers.ar
            1 -> bus_to
            2 -> bus_to.inv()
            3 ->{
                flag_Z = true
                flag_N = false
                0
            }
            4 ->{
                val result = (registers.ar + bus_to).toShort()
                flag_Z = result == 0.toShort()
                flag_N = result < 0
                flag_C = result <= registers.ar
                flag_O = result.getBitAt(6) != registers.ar.getBitAt(6)
                result
            }
            5 ->{
                val result = (registers.ar + bus_to).toShort()
                flag_Z = result == 0.toShort()
                flag_N = result < 0
                flag_C = result <= registers.ar
                result
            }
            6 ->{
                val result = registers.ar.toInt().and(bus_to.toInt()).toShort()
                flag_Z = result == 0.toShort()
                flag_N = result < 0
                result
            }
            7 ->{
                val result = registers.ar.toInt().or(bus_to.toInt()).toShort()
                flag_Z = result == 0.toShort()
                flag_N = result < 0
                result
            }
            8 -> (registers.ar + bus_to).toShort()
            9 ->{
                val result = registers.ar.toInt().shl(1).toShort()
                flag_C = registers.ar.getBitAt(7)
                flag_Z = result == 0.toShort()
                flag_N = result < 0
                result
            }
            10 ->{
                val result = registers.ar.toInt().shl(16).or(registers.hr.toInt().onlyBits(16)).shl(1)
                newRegs.hr = result.onlyBits(16).toShort()
                flag_C = registers.ar.getBitAt(7)
                flag_Z = result == 0
                flag_N = result < 0
                result.ushr(16).toShort()
            }
            11 ->{
                val result = registers.ar.toInt().shr(1).toShort()
                flag_C = registers.ar.getBitAt(0)
                flag_Z = result == 0.toShort()
                flag_N = result < 0
                result
            }
            12 ->{
                val result = registers.ar.toInt().shl(16).or(registers.hr.toInt().onlyBits(16)).shr(1)
                newRegs.hr = result.onlyBits(16).toShort()
                flag_C = registers.hr.getBitAt(0)
                flag_Z = result == 0
                flag_N = result < 0
                result.ushr(16).toShort()
            }
            13 ->{
                val result = registers.ar.toInt().ushr(1).toShort()
                flag_C = registers.ar.getBitAt(0)
                flag_Z = result == 0.toShort()
                flag_N = result < 0
                result
            }
            14 ->{
                val result = registers.ar.toInt().shl(1).or(registers.ar.toInt().ushr(15)).toShort()
                flag_C = registers.ar.getBitAt(7)
                flag_Z = result == 0.toShort()
                flag_N = result < 0
                result
            }
            else ->{
                val arhr = registers.ar.toInt().shl(16).or(registers.hr.toInt().onlyBits(16))
                val result = arhr.shl(1).or(arhr.ushr(31)).toShort()
                flag_C = registers.hr.getBitAt(0)
                flag_Z = result == 0.toShort()
                flag_N = result < 0
                result
            }
        }

        registers = newRegs
    }

    data class Instruction(
            var OP:  Byte = 0,
            var GRx: Byte = 0,
            var M:   Byte = 0,
            var adr: Byte = 0
    ){
        companion object {
            fun parse(irVal: Short) = Instruction(
                    (irVal.toInt() ushr 12 and 15).toByte(),
                    (irVal.toInt() ushr 10 and 3).toByte(),
                    (irVal.toInt() ushr 8 and 3).toByte(),
                    (irVal.toInt() and 255).toByte()
            )
        }
    }

    data class Registers(
            var pc:  Byte  = 0,                 // PC
            var asr: Byte  = 0,                 // ASR
            var ar:  Short = 0,                 // AR
            var hr:  Short = 0,                 // HR
            var gr0: Short = 0,                 // GR0
            var gr1: Short = 0,                 // GR1
            var gr2: Short = 0,                 // GR2
            var gr3: Short = 0,                 // GR3
            var ir:  Short = 0,                 // IR
            var uPC: Byte  = 0,                 // MyPC
            var uSP: Byte  = 0,                 // SMyPC
            var lc:  Byte  = 0                  // LC
    ){
        fun copy() = Registers(pc, asr, ar, hr, gr0, gr1, gr2, gr3, ir, uPC, uSP, lc)
    }
}

fun Short.inv() = toInt().inv().onlyBits(16).toShort()
fun Short.onlyBits(bits: Int) = toInt().onlyBits(bits).toShort()
fun Byte.toMaskedShort() = toProperType(0.toShort())
fun Short.toProperType(other: Short) = this
fun Short.toProperType(other: Byte) = toInt().and(0xFF).toByte()
fun Byte.toProperType(other: Short) = this.toShort()
fun Byte.toProperType(other: Byte) = this
fun Byte.toIndexValue() = toInt() and 0xFF
fun Int.getBitAt(index: Int) = ushr(index).and(1) == 1
fun Short.getBitAt(index: Int) = toInt().ushr(index).and(1) == 1
infix fun Int.onlyBits(bits: Int) = and(-1 ushr (32 - bits))


fun main(args: Array<String>){
    val state = MachineState()
    state.microMemory[0] = 0x2780

    state.step()

    println(state.registers.pc)
    println(state.halted)
}
