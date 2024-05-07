/*
 * Generated by Bluespec Compiler, version 2023.07 (build 0eb551dc)
 * 
 * On Tue May  7 18:00:11 +0530 2024
 * 
 */
#include "bluesim_primitives.h"
#include "model_testFSM.h"

#include <cstdlib>
#include <time.h>
#include "bluesim_kernel_api.h"
#include "bs_vcd.h"
#include "bs_reset.h"


/* Constructor */
MODEL_testFSM::MODEL_testFSM()
{
  testFSM_instance = NULL;
}

/* Function for creating a new model */
void * new_MODEL_testFSM()
{
  MODEL_testFSM *model = new MODEL_testFSM();
  return (void *)(model);
}

/* Schedule functions */

static void schedule_posedge_CLK(tSimStateHdl simHdl, void *instance_ptr)
       {
	 MOD_testFSM &INST_top = *((MOD_testFSM *)(instance_ptr));
	 tUInt8 DEF_INST_top_DEF_CAN_FIRE_RL_runCounter;
	 tUInt8 DEF_INST_top_DEF_WILL_FIRE_RL_runCounter;
	 tUInt8 DEF_INST_top_DEF_CAN_FIRE_RL_stateIdle;
	 tUInt8 DEF_INST_top_DEF_WILL_FIRE_RL_stateIdle;
	 tUInt8 DEF_INST_top_DEF_CAN_FIRE_RL_stateStep1;
	 tUInt8 DEF_INST_top_DEF_WILL_FIRE_RL_stateStep1;
	 tUInt8 DEF_INST_top_DEF_CAN_FIRE_RL_stateStep2;
	 tUInt8 DEF_INST_top_DEF_WILL_FIRE_RL_stateStep2;
	 tUInt8 DEF_INST_top_DEF_CAN_FIRE_RL_stateStep3;
	 tUInt8 DEF_INST_top_DEF_WILL_FIRE_RL_stateStep3;
	 tUInt8 DEF_INST_top_DEF_CAN_FIRE_RL_stateStep4;
	 tUInt8 DEF_INST_top_DEF_WILL_FIRE_RL_stateStep4;
	 tUInt8 DEF_INST_top_DEF_CAN_FIRE_RL_stateStep5;
	 tUInt8 DEF_INST_top_DEF_WILL_FIRE_RL_stateStep5;
	 tUInt8 DEF_INST_top_DEF_CAN_FIRE_RL_stateStep6;
	 tUInt8 DEF_INST_top_DEF_WILL_FIRE_RL_stateStep6;
	 tUInt8 DEF_INST_top_DEF_CAN_FIRE_RL_stateStep7;
	 tUInt8 DEF_INST_top_DEF_WILL_FIRE_RL_stateStep7;
	 tUInt8 DEF_INST_top_DEF_CAN_FIRE_RL_stateStep8;
	 tUInt8 DEF_INST_top_DEF_WILL_FIRE_RL_stateStep8;
	 tUInt8 DEF_INST_top_DEF_CAN_FIRE_RL_stateStep9;
	 tUInt8 DEF_INST_top_DEF_WILL_FIRE_RL_stateStep9;
	 tUInt8 DEF_INST_top_DEF_CAN_FIRE_RL_stateStep10;
	 tUInt8 DEF_INST_top_DEF_WILL_FIRE_RL_stateStep10;
	 tUInt8 DEF_INST_top_DEF_CAN_FIRE_RL_stateStep11;
	 tUInt8 DEF_INST_top_DEF_WILL_FIRE_RL_stateStep11;
	 tUInt8 DEF_INST_top_DEF_CAN_FIRE_RL_stateStep12;
	 tUInt8 DEF_INST_top_DEF_WILL_FIRE_RL_stateStep12;
	 tUInt8 DEF_INST_top_DEF_CAN_FIRE_RL_stateStep13;
	 tUInt8 DEF_INST_top_DEF_WILL_FIRE_RL_stateStep13;
	 tUInt8 DEF_INST_top_DEF_CAN_FIRE_RL_stateSTOP;
	 tUInt8 DEF_INST_top_DEF_WILL_FIRE_RL_stateSTOP;
	 DEF_INST_top_DEF_CAN_FIRE_RL_runCounter = (tUInt8)1u;
	 DEF_INST_top_DEF_WILL_FIRE_RL_runCounter = DEF_INST_top_DEF_CAN_FIRE_RL_runCounter;
	 INST_top.DEF_state__h936 = INST_top.INST_state.METH_read();
	 DEF_INST_top_DEF_CAN_FIRE_RL_stateIdle = (INST_top.DEF_state__h936) == (tUInt8)0u;
	 DEF_INST_top_DEF_WILL_FIRE_RL_stateIdle = DEF_INST_top_DEF_CAN_FIRE_RL_stateIdle;
	 DEF_INST_top_DEF_CAN_FIRE_RL_stateSTOP = (INST_top.DEF_state__h936) == (tUInt8)14u;
	 DEF_INST_top_DEF_WILL_FIRE_RL_stateSTOP = DEF_INST_top_DEF_CAN_FIRE_RL_stateSTOP;
	 DEF_INST_top_DEF_CAN_FIRE_RL_stateStep1 = (INST_top.DEF_state__h936) == (tUInt8)1u;
	 DEF_INST_top_DEF_WILL_FIRE_RL_stateStep1 = DEF_INST_top_DEF_CAN_FIRE_RL_stateStep1;
	 DEF_INST_top_DEF_CAN_FIRE_RL_stateStep10 = (INST_top.DEF_state__h936) == (tUInt8)10u;
	 DEF_INST_top_DEF_WILL_FIRE_RL_stateStep10 = DEF_INST_top_DEF_CAN_FIRE_RL_stateStep10;
	 DEF_INST_top_DEF_CAN_FIRE_RL_stateStep11 = (INST_top.DEF_state__h936) == (tUInt8)11u;
	 DEF_INST_top_DEF_WILL_FIRE_RL_stateStep11 = DEF_INST_top_DEF_CAN_FIRE_RL_stateStep11;
	 DEF_INST_top_DEF_CAN_FIRE_RL_stateStep12 = (INST_top.DEF_state__h936) == (tUInt8)12u;
	 DEF_INST_top_DEF_WILL_FIRE_RL_stateStep12 = DEF_INST_top_DEF_CAN_FIRE_RL_stateStep12;
	 DEF_INST_top_DEF_CAN_FIRE_RL_stateStep13 = (INST_top.DEF_state__h936) == (tUInt8)13u;
	 DEF_INST_top_DEF_WILL_FIRE_RL_stateStep13 = DEF_INST_top_DEF_CAN_FIRE_RL_stateStep13;
	 DEF_INST_top_DEF_CAN_FIRE_RL_stateStep2 = (INST_top.DEF_state__h936) == (tUInt8)2u;
	 DEF_INST_top_DEF_WILL_FIRE_RL_stateStep2 = DEF_INST_top_DEF_CAN_FIRE_RL_stateStep2;
	 DEF_INST_top_DEF_CAN_FIRE_RL_stateStep3 = (INST_top.DEF_state__h936) == (tUInt8)3u;
	 DEF_INST_top_DEF_WILL_FIRE_RL_stateStep3 = DEF_INST_top_DEF_CAN_FIRE_RL_stateStep3;
	 DEF_INST_top_DEF_CAN_FIRE_RL_stateStep4 = (INST_top.DEF_state__h936) == (tUInt8)4u;
	 DEF_INST_top_DEF_WILL_FIRE_RL_stateStep4 = DEF_INST_top_DEF_CAN_FIRE_RL_stateStep4;
	 DEF_INST_top_DEF_CAN_FIRE_RL_stateStep6 = (INST_top.DEF_state__h936) == (tUInt8)6u;
	 DEF_INST_top_DEF_WILL_FIRE_RL_stateStep6 = DEF_INST_top_DEF_CAN_FIRE_RL_stateStep6;
	 DEF_INST_top_DEF_CAN_FIRE_RL_stateStep5 = (INST_top.DEF_state__h936) == (tUInt8)5u;
	 DEF_INST_top_DEF_WILL_FIRE_RL_stateStep5 = DEF_INST_top_DEF_CAN_FIRE_RL_stateStep5;
	 DEF_INST_top_DEF_CAN_FIRE_RL_stateStep7 = (INST_top.DEF_state__h936) == (tUInt8)7u;
	 DEF_INST_top_DEF_WILL_FIRE_RL_stateStep7 = DEF_INST_top_DEF_CAN_FIRE_RL_stateStep7;
	 DEF_INST_top_DEF_CAN_FIRE_RL_stateStep8 = (INST_top.DEF_state__h936) == (tUInt8)8u;
	 DEF_INST_top_DEF_WILL_FIRE_RL_stateStep8 = DEF_INST_top_DEF_CAN_FIRE_RL_stateStep8;
	 DEF_INST_top_DEF_CAN_FIRE_RL_stateStep9 = (INST_top.DEF_state__h936) == (tUInt8)9u;
	 DEF_INST_top_DEF_WILL_FIRE_RL_stateStep9 = DEF_INST_top_DEF_CAN_FIRE_RL_stateStep9;
	 INST_top.DEF_WILL_FIRE_addr_csr = INST_top.PORT_EN_addr_csr;
	 INST_top.METH_RDY_addr_csr();
	 INST_top.DEF_WILL_FIRE_addr_len_csr = INST_top.PORT_EN_addr_len_csr;
	 INST_top.METH_RDY_addr_len_csr();
	 INST_top.DEF_WILL_FIRE_addr_valid_byte_csr = INST_top.PORT_EN_addr_valid_byte_csr;
	 INST_top.METH_RDY_addr_valid_byte_csr();
	 INST_top.DEF_WILL_FIRE_outClock = INST_top.PORT_EN_outClock;
	 INST_top.METH_RDY_outClock();
	 INST_top.DEF_WILL_FIRE_rd_fifo = INST_top.PORT_EN_rd_fifo;
	 INST_top.METH_RDY_rd_fifo();
	 INST_top.DEF_WILL_FIRE_read_write_csr = INST_top.PORT_EN_read_write_csr;
	 INST_top.METH_RDY_read_write_csr();
	 INST_top.DEF_WILL_FIRE_reset = INST_top.PORT_EN_reset;
	 INST_top.METH_RDY_reset();
	 INST_top.DEF_WILL_FIRE_slvaddr_csr = INST_top.PORT_EN_slvaddr_csr;
	 INST_top.METH_RDY_slvaddr_csr();
	 INST_top.DEF_WILL_FIRE_start_csr = INST_top.PORT_EN_start_csr;
	 INST_top.METH_RDY_start_csr();
	 INST_top.DEF_WILL_FIRE_wr_fifo_csr = INST_top.PORT_EN_wr_fifo_csr;
	 INST_top.METH_RDY_wr_fifo_csr();
	 if (INST_top.DEF_WILL_FIRE_addr_csr)
	   INST_top.METH_addr_csr(INST_top.PORT_addr_csr_addr);
	 if (INST_top.DEF_WILL_FIRE_addr_len_csr)
	   INST_top.METH_addr_len_csr(INST_top.PORT_addr_len_csr_addr_len);
	 if (INST_top.DEF_WILL_FIRE_addr_valid_byte_csr)
	   INST_top.METH_addr_valid_byte_csr(INST_top.PORT_addr_valid_byte_csr_addr_valid_byte);
	 if (DEF_INST_top_DEF_WILL_FIRE_RL_stateStep1)
	   INST_top.RL_stateStep1();
	 if (DEF_INST_top_DEF_WILL_FIRE_RL_stateStep11)
	   INST_top.RL_stateStep11();
	 if (DEF_INST_top_DEF_WILL_FIRE_RL_stateStep12)
	   INST_top.RL_stateStep12();
	 if (DEF_INST_top_DEF_WILL_FIRE_RL_stateStep3)
	   INST_top.RL_stateStep3();
	 if (DEF_INST_top_DEF_WILL_FIRE_RL_stateStep5)
	   INST_top.RL_stateStep5();
	 if (DEF_INST_top_DEF_WILL_FIRE_RL_stateStep6)
	   INST_top.RL_stateStep6();
	 if (DEF_INST_top_DEF_WILL_FIRE_RL_stateStep7)
	   INST_top.RL_stateStep7();
	 if (DEF_INST_top_DEF_WILL_FIRE_RL_stateStep9)
	   INST_top.RL_stateStep9();
	 if (INST_top.DEF_WILL_FIRE_read_write_csr)
	   INST_top.METH_read_write_csr(INST_top.PORT_read_write_csr_read_write);
	 if (INST_top.DEF_WILL_FIRE_reset)
	   INST_top.METH_reset(INST_top.PORT_reset_value);
	 if (INST_top.DEF_WILL_FIRE_outClock)
	   INST_top.METH_outClock(INST_top.PORT_outClock_value);
	 if (DEF_INST_top_DEF_WILL_FIRE_RL_stateIdle)
	   INST_top.RL_stateIdle();
	 if (DEF_INST_top_DEF_WILL_FIRE_RL_stateSTOP)
	   INST_top.RL_stateSTOP();
	 if (DEF_INST_top_DEF_WILL_FIRE_RL_stateStep10)
	   INST_top.RL_stateStep10();
	 if (DEF_INST_top_DEF_WILL_FIRE_RL_stateStep13)
	   INST_top.RL_stateStep13();
	 if (DEF_INST_top_DEF_WILL_FIRE_RL_stateStep2)
	   INST_top.RL_stateStep2();
	 if (DEF_INST_top_DEF_WILL_FIRE_RL_stateStep4)
	   INST_top.RL_stateStep4();
	 if (DEF_INST_top_DEF_WILL_FIRE_RL_stateStep8)
	   INST_top.RL_stateStep8();
	 if (DEF_INST_top_DEF_WILL_FIRE_RL_runCounter)
	   INST_top.RL_runCounter();
	 if (INST_top.DEF_WILL_FIRE_slvaddr_csr)
	   INST_top.METH_slvaddr_csr(INST_top.PORT_slvaddr_csr_slvaddr);
	 if (INST_top.DEF_WILL_FIRE_start_csr)
	   INST_top.METH_start_csr(INST_top.PORT_start_csr_start);
	 if (INST_top.DEF_WILL_FIRE_wr_fifo_csr)
	   INST_top.METH_wr_fifo_csr(INST_top.PORT_wr_fifo_csr_wr_fifo);
	 if (INST_top.DEF_WILL_FIRE_rd_fifo)
	   INST_top.METH_rd_fifo();
	 if (do_reset_ticks(simHdl))
	 {
	   INST_top.INST_f_in.rst_tick_clk((tUInt8)1u);
	   INST_top.INST_f_out.rst_tick_clk((tUInt8)1u);
	   INST_top.INST_state.rst_tick__clk__1((tUInt8)1u);
	   INST_top.INST_counter.rst_tick__clk__1((tUInt8)1u);
	   INST_top.INST_startReg.rst_tick__clk__1((tUInt8)1u);
	   INST_top.INST_slvaddrReg.rst_tick__clk__1((tUInt8)1u);
	   INST_top.INST_addr_lenReg.rst_tick__clk__1((tUInt8)1u);
	   INST_top.INST_read_writeReg.rst_tick__clk__1((tUInt8)1u);
	   INST_top.INST_addrReg.rst_tick__clk__1((tUInt8)1u);
	   INST_top.INST_addr_valid_byteReg.rst_tick__clk__1((tUInt8)1u);
	   INST_top.INST_wr_fifo_emptyReg.rst_tick__clk__1((tUInt8)1u);
	   INST_top.INST_wr_fifo_fullReg.rst_tick__clk__1((tUInt8)1u);
	   INST_top.INST_rd_fifo_emptyReg.rst_tick__clk__1((tUInt8)1u);
	   INST_top.INST_rd_fifo_fullReg.rst_tick__clk__1((tUInt8)1u);
	   INST_top.INST_wr_overflowReg.rst_tick__clk__1((tUInt8)1u);
	   INST_top.INST_rd_overflowReg.rst_tick__clk__1((tUInt8)1u);
	   INST_top.INST_nack_errorReg.rst_tick__clk__1((tUInt8)1u);
	   INST_top.INST_underflow_errorReg.rst_tick__clk__1((tUInt8)1u);
	   INST_top.INST_bus_errorReg.rst_tick__clk__1((tUInt8)1u);
	   INST_top.INST_i2c_busyReg.rst_tick__clk__1((tUInt8)1u);
	   INST_top.INST_sdaReg.rst_tick__clk__1((tUInt8)1u);
	   INST_top.INST_sclReg.rst_tick__clk__1((tUInt8)1u);
	 }
       };

/* Model creation/destruction functions */

void MODEL_testFSM::create_model(tSimStateHdl simHdl, bool master)
{
  sim_hdl = simHdl;
  init_reset_request_counters(sim_hdl);
  testFSM_instance = new MOD_testFSM(sim_hdl, "top", NULL);
  bk_get_or_define_clock(sim_hdl, "CLK");
  if (master)
  {
    bk_alter_clock(sim_hdl, bk_get_clock_by_name(sim_hdl, "CLK"), CLK_LOW, false, 0llu, 5llu, 5llu);
    bk_use_default_reset(sim_hdl);
  }
  bk_set_clock_event_fn(sim_hdl,
			bk_get_clock_by_name(sim_hdl, "CLK"),
			schedule_posedge_CLK,
			NULL,
			(tEdgeDirection)(POSEDGE));
  (testFSM_instance->INST_f_in.set_clk_0)("CLK");
  (testFSM_instance->INST_f_out.set_clk_0)("CLK");
  (testFSM_instance->set_clk_0)("CLK");
}
void MODEL_testFSM::destroy_model()
{
  delete testFSM_instance;
  testFSM_instance = NULL;
}
void MODEL_testFSM::reset_model(bool asserted)
{
  (testFSM_instance->reset_RST_N)(asserted ? (tUInt8)0u : (tUInt8)1u);
}
void * MODEL_testFSM::get_instance()
{
  return testFSM_instance;
}

/* Fill in version numbers */
void MODEL_testFSM::get_version(char const **name, char const **build)
{
  *name = "2023.07";
  *build = "0eb551dc";
}

/* Get the model creation time */
time_t MODEL_testFSM::get_creation_time()
{
  
  /* Tue May  7 12:30:11 UTC 2024 */
  return 1715085011llu;
}

/* State dumping function */
void MODEL_testFSM::dump_state()
{
  (testFSM_instance->dump_state)(0u);
}

/* VCD dumping functions */
MOD_testFSM & testFSM_backing(tSimStateHdl simHdl)
{
  static MOD_testFSM *instance = NULL;
  if (instance == NULL)
  {
    vcd_set_backing_instance(simHdl, true);
    instance = new MOD_testFSM(simHdl, "top", NULL);
    vcd_set_backing_instance(simHdl, false);
  }
  return *instance;
}
void MODEL_testFSM::dump_VCD_defs()
{
  (testFSM_instance->dump_VCD_defs)(vcd_depth(sim_hdl));
}
void MODEL_testFSM::dump_VCD(tVCDDumpType dt)
{
  (testFSM_instance->dump_VCD)(dt, vcd_depth(sim_hdl), testFSM_backing(sim_hdl));
}
