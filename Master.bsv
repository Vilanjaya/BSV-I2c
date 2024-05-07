package Master;

import FIFOF::*;
import GetPut::*;
import Vector::*;
import StmtFSM::*;

interface Master_ifc;
    method Action start_csr (Bool start);
    method Action slvaddr_csr (Int#(10) slvaddr);
    method Action addr_len_csr (Bool addr_len);
    method Action read_write_csr (Bool read_write);
    method Action addr_csr (Int#(32) addr);
    method Action addr_valid_byte_csr (Int#(2) addr_valid_byte);

    method Bool rd_fifo_empty;
    method Bool rd_fifo_full;
    method Bool wr_fifo_empty;
    method Bool wr_fifo_full;
    method Bool rd_overflow;
    method Bool wr_overflow;
    method Bool underflow_error;
    method Bool nack_error;
    method Bool bus_error;
    method Bool i2c_busy;

    method Bool sda;
    method Bool scl;

    method State read();

    method Action wr_fifo_csr (Int#(8) wr_fifo);
    method ActionValue#(Int#(8)) rd_fifo();

    method Action reset(Bool value);
    method Action outClock(Bool value); //is the scl clk signal

endinterface:Master_ifc

typedef enum {
    START,
    SEND_DEV_ADDR,
    R_W,
    ACKNOWLEDGE_1,
    SEND_REG_ADDR,
    ACKNOWLEDGE_2,
    WRITE_DATA,
    ACKNOWLEDGE_4,
    RESTART,
    SEND_DEV_ADDR_2,
    R_W_2,
    ACKNOWLEDGE_3,
    RECEIVE_DATA,
    NACK,
    STOP 
} State deriving(Bits,Eq);

(* synthesize *)
module testFSM(Master_ifc);

    FIFOF#(Int#(8)) f_in  <- mkFIFOF;    // to buffer incoming requests
    FIFOF#(Int#(8)) f_out <- mkFIFOF;    // to buffer outgoing responses

    Reg#(State) state <- mkReg(START);
    Reg#(int) counter <- mkReg(0);
    Reg#(Bool) startReg <- mkReg(False);
    Reg#(Int#(10)) slvaddrReg <- mkReg(0);
    Reg#(Bool) addr_lenReg <- mkReg(False);
    Reg#(Bool) read_writeReg <- mkReg(False);
    Reg#(int) addrReg <- mkReg(0);
    Reg#(Int#(2)) addr_valid_byteReg <- mkReg(0);

    Reg#(Bool) wr_fifo_emptyReg <- mkReg(True);
    Reg#(Bool) wr_fifo_fullReg <- mkReg(False);
    Reg#(Bool) rd_fifo_emptyReg <- mkReg(True);
    Reg#(Bool) rd_fifo_fullReg <- mkReg(False);
    Reg#(Bool) wr_overflowReg <- mkReg(False);
    Reg#(Bool) rd_overflowReg <- mkReg(False);
    Reg#(Bool) nack_errorReg <- mkReg(False);
    Reg#(Bool) underflow_errorReg <- mkReg(False);
    Reg#(Bool) bus_errorReg <- mkReg(False);
    Reg#(Bool) i2c_busyReg <- mkReg(False);

    Reg#(Bool) sdaReg <- mkReg(False);
    Reg#(Bool) sclReg <- mkReg(False);


    rule runCounter;
        if (counter == 100) begin
           $display("Done");
           $finish;
        end else if (counter < 100) begin
        counter <= counter + 1;
        end 
     endrule

    rule stateIdle ( state == START ); //default state
     $display("Counter = %3d, State: START", counter);
     if (startReg == True)
        state <= SEND_DEV_ADDR;
        sdaReg <= True; // Setting sda to 1

    endrule

    rule stateStep1(state == SEND_DEV_ADDR);
        $display("Counter = %3d, State: SEND_DEV_ADDR", counter);
        if (addr_lenReg) begin
            for (Integer i = 0; i < 10; i = i + 1) begin
                // sdaReg <= slvaddrReg;
            end
            state <= R_W;
        end else begin
            for (Integer i = 0; i < 7; i = i + 1) begin
                // sdaReg <= True ;
            end
            state <= R_W;
        end
    endrule

    rule stateStep2 ( state == R_W );
        $display("Counter = %3d, State: R_W", counter);
        sdaReg<=False;
        state <= ACKNOWLEDGE_1;
    endrule

    rule stateStep3 ( state == ACKNOWLEDGE_1 );
        $display("Counter = %3d, State: ACKNOWLEDGE_1", counter);
        if(sdaReg) begin
            state <= SEND_REG_ADDR;
        end else begin
            nack_errorReg <= True;
        end
        
    endrule

    rule stateStep4 ( state == SEND_REG_ADDR );
        $display("Counter = %3d, State: SEND_REG_ADDR", counter);
        state <= ACKNOWLEDGE_2;
        sdaReg<=True;
    endrule

    rule stateStep5 ( state == ACKNOWLEDGE_2 );
        $display("Counter = %3d, State: ACKNOWLEDGE_2", counter);
        if (sdaReg) begin
                    if (!read_writeReg) begin
                        state <= WRITE_DATA;
                    end else if (read_writeReg) begin
                        state <= RESTART;
                    end
        end
    endrule

    rule stateStep6 ( state == WRITE_DATA );
        $display("Counter = %3d, State: WRITE_DATA", counter);
        
            if (!wr_fifo_emptyReg) begin
                for (Integer i = 0; i < 8; i = i + 1) begin
                    // sdaReg <= f_in.deq;
                end
                state <= ACKNOWLEDGE_4;
        end
        
    endrule

    rule stateStep7 ( state == ACKNOWLEDGE_4 );
        $display("Counter = %3d, State: ACKNOWLEDGE_4", counter);
        if (sdaReg) begin
            state <= STOP; 
        end 
    endrule

    rule stateStep8 ( state == RESTART );
        $display("Counter = %3d, State: RESTART", counter);
        state <= SEND_DEV_ADDR_2;
        sdaReg <=True;
    endrule

    rule stateStep9 ( state == SEND_DEV_ADDR_2 );
        $display("Counter = %3d, State: SEND_DEV_ADDR_2", counter);
        if (addr_lenReg) begin
            for (Integer i = 0; i < 10; i = i + 1) begin
                // sdaReg <= slvaddrReg;
            end
        state <= R_W_2;
        end else begin
            for (Integer i = 0; i < 7; i = i + 1) begin
                // sdaReg <= True ;
            end
            state <= R_W_2;
        end
    endrule

        rule stateStep10 ( state == R_W_2 );
        $display("Counter = %3d, State: STEP2", counter);
        state <= ACKNOWLEDGE_3;
        sdaReg<=True;
    endrule

    rule stateStep11 ( state == ACKNOWLEDGE_3 );
        $display("Counter = %3d, State: R_W_2", counter);
        if (sdaReg)begin        
            state <= RECEIVE_DATA;
        end
    endrule

    rule stateStep12 ( state == RECEIVE_DATA );
        $display("Counter = %3d, State: RECEIVE_DATA", counter);
        for (Integer i = 0; i < 7; i = i + 1) begin
            // f_out.enq <=sdaReg;
            
        end
        state <= NACK;
    endrule

    rule stateStep13 ( state == NACK );
        $display("Counter = %3d, State: NACK", counter);
        sdaReg<=True;
        state <= STOP;
    endrule

    rule stateSTOP ( state == STOP );
        $display("Counter = %3d, State: STOP", counter);
        state <= START;
        sdaReg<=True;
    endrule

    

    method Action start_csr(Bool start);
        startReg <= start;
    endmethod

    method Action slvaddr_csr (Int#(10) slvaddr);
        slvaddrReg <= slvaddr;
    endmethod

    method Action addr_len_csr (Bool addr_len);
        addr_lenReg <= addr_len;
    endmethod

    method Action read_write_csr (Bool read_write);
        read_writeReg <= read_write;
    endmethod

    method Action addr_csr (Int#(32) addr);
        addrReg <= addr;
    endmethod

    method Action addr_valid_byte_csr (Int#(2) addr_valid_byte);
        addr_valid_byteReg <= addr_valid_byte;
    endmethod

    method Bool rd_fifo_empty;
        return rd_fifo_emptyReg;
    endmethod
    method Bool rd_fifo_full;
        return rd_fifo_fullReg;
    endmethod
    method Bool wr_fifo_empty;
        return wr_fifo_emptyReg;
    endmethod
    method Bool wr_fifo_full;
        return wr_fifo_fullReg;
    endmethod
    method Bool rd_overflow;
        return rd_overflowReg;
    endmethod
    method Bool wr_overflow;
        return wr_overflowReg;
    endmethod
    method Bool underflow_error;
        return underflow_errorReg;
    endmethod
    method Bool nack_error;
        return nack_errorReg;
    endmethod
    method Bool bus_error;
        return bus_errorReg;
    endmethod
    method Bool i2c_busy;
        return i2c_busyReg;
    endmethod
    method Bool sda;
        return sdaReg;
    endmethod
    method Bool scl;
        return sclReg;
    endmethod
    method State read();
        return state;
     endmethod


    method Action wr_fifo_csr (Int#(8) wr_fifo);
        f_in.enq (wr_fifo);
        rd_fifo_emptyReg <=f_in.notFull;
        rd_fifo_fullReg <=f_in.notEmpty;
        wr_fifo_emptyReg <=f_out.notFull;
        wr_fifo_fullReg <=f_out.notEmpty;
    endmethod
 
    method ActionValue#(Int#(8)) rd_fifo();
        f_out.deq; return f_out.first;
    endmethod
    method Action reset(Bool value);
        if (value) begin
        sclReg <= False;
        end
    endmethod

    method Action outClock(Bool value);
        sclReg <= value;
    endmethod

endmodule: testFSM
endpackage
