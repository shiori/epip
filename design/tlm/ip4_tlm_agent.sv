///`include "ip4_tlm.svh"

class ip4_tlm_driver #(type T=ovm_sequence_item) extends ovm_driver #(T);
  virtual tlm_sys_if.mods sysif;
  `ovm_component_param_utils(ip4_tlm_driver#(T))

  ovm_nonblocking_transport_port#(T, T) tr_port;
  
  function new(string name, ovm_component parent);
    super.new(name, parent);
  endfunction : new
  
  virtual function void build();
    ovm_object tmp;
    tlm_vif_object vif_cfg;  
    super.build();
    tr_port = new("port", this);
    no_virtual_interface: assert(get_config_object("vif_cfg", tmp));
    failed_convert_interface: assert($cast(vif_cfg, tmp));
    sysif = vif_cfg.get_vif();      
  endfunction : build
  
  task run();
    time run_delay;
    T req, rsp;
    assert(get_config_int("run_delay", run_delay));
    #run_delay;    
    forever begin
      @(posedge sysif.clk);
      seq_item_port.get_next_item(req);
      ovm_report_info("DR", "Driving...", OVM_HIGH);
      ovm_report_info("DR", $psprintf("Detail:\n%s", req.sprint()), OVM_FULL);  
      void'(tr_port.nb_transport(req, rsp));
      seq_item_port.item_done();
    end
  endtask : run
endclass : ip4_tlm_driver

class ip4_tlm_monitor #(type T=ovm_sequence_item) extends ovm_driver #(T);
  virtual tlm_sys_if.mods sysif;
  ovm_analysis_port #(T) item_collected_port;

  `ovm_component_param_utils(ip4_tlm_monitor#(T))

  ovm_nonblocking_transport_imp #(T, T, ip4_tlm_monitor#(T)) tr_imp;
  
  function new(string name, ovm_component parent);
    super.new(name, parent);
  endfunction : new
  
  virtual function void build();
    ovm_object tmp;
    tlm_vif_object vif_cfg;  
    super.build();
    tr_imp = new("imp", this);
    item_collected_port = new("item_collected_port", this);
    no_virtual_interface: assert(get_config_object("vif_cfg", tmp));
    failed_convert_interface: assert($cast(vif_cfg, tmp));
    sysif = vif_cfg.get_vif();      
  endfunction : build
  
  task run();
    forever begin
      @(posedge sysif.clk);
    end
  endtask : run
  
  function bit nb_transport(input T req, output T rsp);
///    rsp = tr_spa2rfm::type_id::create("spa_rsp", this);
///    void'(rsp.randomize());
    rsp = req;
    ovm_report_info("MON", "Got Transaction...", OVM_HIGH);
    ovm_report_info("MON", $psprintf("Detail:\n%s", req.sprint()), OVM_FULL);
    item_collected_port.write(req);
    return 1;
  endfunction : nb_transport
endclass : ip4_tlm_monitor

class ip4_tlm_sequencer #(type T=ovm_sequence_item) extends ovm_sequencer #(T);
  `ovm_sequencer_param_utils(ip4_tlm_sequencer#(T))
  
  function new(string name, ovm_component parent=null);
    super.new(name, parent);
    `ovm_update_sequence_lib_and_item(T)
  endfunction /// new

endclass : ip4_tlm_sequencer

class ip4_tlm_agent #(type TD=ovm_sequence_item, TM=ovm_sequence_item) extends ovm_agent;
  ovm_active_passive_enum is_active = OVM_ACTIVE;
  `ovm_component_param_utils_begin(ip4_tlm_agent#(TD, TM))
    `ovm_field_enum(ovm_active_passive_enum, is_active, OVM_ALL_ON)
  `ovm_component_utils_end
  
  ip4_tlm_sequencer#(TD) sequencer;
  ip4_tlm_monitor#(TM) monitor;
  ip4_tlm_driver#(TD) driver;
  
  function new(string name, ovm_component parent=null);
    super.new(name, parent);
  endfunction /// new
  
  virtual function void build();
    super.build();
    monitor = ip4_tlm_monitor#(TM)::type_id::create("monitor",this);
    if (is_active == OVM_ACTIVE) begin
      sequencer = ip4_tlm_sequencer#(TD)::type_id::create("sequencer",this);
      driver = ip4_tlm_driver#(TD)::type_id::create("driver",this);
    end
  endfunction : build
  
  virtual function void connect();
    super.connect();
    if(is_active == OVM_ACTIVE) begin
      driver.seq_item_port.connect(sequencer.seq_item_export);
    end
  endfunction : connect
endclass : ip4_tlm_agent
