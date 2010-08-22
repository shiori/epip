// $Id: //dvt/vtech/proj/transactions/ovm/src/base/ovm_misc.sv#1 $
//----------------------------------------------------------------------
//   Copyright 2007-2008 Mentor Graphics Corporation
//   Copyright 2007-2008 Cadence Design Systems, Inc.
//   All Rights Reserved Worldwide
//
//   Licensed under the Apache License, Version 2.0 (the
//   "License"); you may not use this file except in
//   compliance with the License.  You may obtain a copy of
//   the License at
//
//       http://www.apache.org/licenses/LICENSE-2.0
//
//   Unless required by applicable law or agreed to in
//   writing, software distributed under the License is
//   distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR
//   CONDITIONS OF ANY KIND, either express or implied.  See
//   the License for the specific language governing
//   permissions and limitations under the License.
//----------------------------------------------------------------------

// Copied from base/ovm_misc.sv

// OVM does not provide any kind of recording functionality, but provides hooks
// when a component/object may need such a hook.

`ifndef OVM_RECORD_INTERFACE
`define OVM_RECORD_INTERFACE

/* A-Z and a-z and _ */
bit ovm_tr_is_letter[0:127] = '{
0, 0, 0, 0, 0, 0, 0, 0, /*   0 -   7 */
0, 0, 0, 0, 0, 0, 0, 0, /*   8 -  15 */
0, 0, 0, 0, 0, 0, 0, 0, /*  16 -  23 */
0, 0, 0, 0, 0, 0, 0, 0, /*  24 -  31 */
0, 0, 0, 0, 0, 0, 0, 0, /*  32 -  39 */
0, 0, 0, 0, 0, 0, 0, 0, /*  40 -  47 */ 
0, 0, 0, 0, 0, 0, 0, 0, /*  48 -  55 */ /* 48 = 0 */
0, 0, 0, 0, 0, 0, 0, 0, /*  56 -  63 */ /* 57 = 9 */
0, 1, 1, 1, 1, 1, 1, 1, /*  64 -  71 */ /* 65 = A */
1, 1, 1, 1, 1, 1, 1, 1, /*  72 -  79 */
1, 1, 1, 1, 1, 1, 1, 1, /*  80 -  87 */
1, 1, 1, 0, 0, 0, 0, 1, /*  88 -  95 */ /* 90 = Z, 95 = _ */
0, 1, 1, 1, 1, 1, 1, 1, /*  96 - 103 */ /* 97 = a */
1, 1, 1, 1, 1, 1, 1, 1, /* 104 - 111 */
1, 1, 1, 1, 1, 1, 1, 1, /* 112 - 119 */
1, 1, 1, 0, 0, 0, 0, 0  /* 120 - 127 */ /* 122 = z */
};


bit ovm_tr_is_legal[0:127] = '{
0, 0, 0, 0, 0, 0, 0, 0, /*   0 -   7 */
0, 0, 0, 0, 0, 0, 0, 0, /*   8 -  15 */
0, 0, 0, 0, 0, 0, 0, 0, /*  16 -  23 */
0, 0, 0, 0, 0, 0, 0, 0, /*  24 -  31 */
0, 0, 0, 0, 0, 0, 0, 0, /*  32 -  39 */
0, 0, 0, 0, 0, 0, 0, 0, /*  40 -  47 */ 
1, 1, 1, 1, 1, 1, 1, 1, /*  48 -  55 */ /* 48 = 0 */
1, 1, 0, 0, 0, 0, 0, 0, /*  56 -  63 */ /* 57 = 9 */
0, 1, 1, 1, 1, 1, 1, 1, /*  64 -  71 */ /* 65 = A */
1, 1, 1, 1, 1, 1, 1, 1, /*  72 -  79 */
1, 1, 1, 1, 1, 1, 1, 1, /*  80 -  87 */
1, 1, 1, 0, 0, 0, 0, 1, /*  88 -  95 */ /* 90 = Z */
0, 1, 1, 1, 1, 1, 1, 1, /*  96 - 103 */ /* 97 = a */
1, 1, 1, 1, 1, 1, 1, 1, /* 104 - 111 */
1, 1, 1, 1, 1, 1, 1, 1, /* 112 - 119 */
1, 1, 1, 0, 0, 0, 0, 0  /* 120 - 127 */ /* 122 = z */
};


/*
 * ovm_tr_make_legal_c_identifier()
 *
 * Given an input string, s, return a string 
 * which contains the original s, with
 * all illegal characters replaced with underscore.
 *
 * Algorithm: 
 *  First character should be a letter.
 *  All remaining characters should be letter, number or
 *  underscore.
 */
function string ovm_tr_make_legal_c_identifier(string s);
  for (int i = 0; i < s.len(); i++) begin
    if (s[i] > 127)
        s[i] = "_";
    else
      if (i == 0) begin
        if (!ovm_tr_is_letter[s[i]])
          s[i] = "_";
      end
      else begin
        if (!ovm_tr_is_legal[s[i]])
          s[i] = "_";
      end
  end
  return s;
endfunction


typedef longint TRH;
typedef longint STRH;

  //TODO: Shouldn't this be elsewhere?
  //      These exist solely to support ovm_check_handle_kind()
  //      which is likely an unneeded call.
  static STRH        streamhandles  [string];
  static string      streamhandles_r[STRH];
  static TRH    transactionhandles  [string];
  static string transactionhandles_r[TRH];

  /* 
   * NameOfStream()
   * 
   * Useful for debug - maps a handle back to a name
   */
  function static string NameOfStream(STRH h);
    string name = "<Unknown>";
    if (streamhandles_r.exists(h))
      name = streamhandles_r[h];
    return name;
  endfunction

  /* 
   * NameOfTransaction()
   * 
   * Useful for debug - maps a handle back to a name
   */
  function static string NameOfTransaction(TRH h);
    string name = "<Unknown>";
    if (transactionhandles_r.exists(h))
      name = transactionhandles_r[h];
    return name;
  endfunction



  /* 
   * Debugging and tracing interface 
   *
   * Three levels of debug:
   *
   *         OVM_tr_trACE_API - Trace the OVM calls
   *  OVM_tr_trACE_API_DETAIL - Details in the OVM calls
   *   OVM_tr_trACE_PLI_CALLS - Print the actual $ calls that will be made
   *
   * Use with VSIM:
   *   vsim +OVM_tr_trACE_API        -c top
   *   vsim +OVM_tr_trACE_API_DETAIL -c top
   *   vsim +OVM_tr_trACE_PLI_CALLS  -c top
   *
   */

  static int OVM_tr_trACE_API = 0;
  static int OVM_tr_trACE_API_DETAIL = 0;
  static int OVM_tr_trACE_PLI_CALLS = 0;

  static int ovm_tr_initialized = 0;

  function void ovm_tr_init();

    OVM_tr_trACE_API = 0;
    OVM_tr_trACE_API_DETAIL = 0;
    OVM_tr_trACE_PLI_CALLS = 0;

    if ($test$plusargs("OVM_tr_trACE_API"))
      OVM_tr_trACE_API = 1;
    if ($test$plusargs("OVM_tr_trACE_API_DETAIL")) begin
      OVM_tr_trACE_API = 1;
      OVM_tr_trACE_API_DETAIL = 1;
    end
    if ($test$plusargs("OVM_tr_trACE_PLI_CALLS"))
      OVM_tr_trACE_PLI_CALLS = 1;

    ovm_tr_initialized = 1;

    $display("**************************************************************");
    $display("* BETA Version of Questa OVM Transaction Recording Turned ON *");
    $display("*  to turn off, set 'recording_detail' to off                *");
    $display("*   set_config_int(\"*\", \"recording_detail\", 0);              *");
    $display("**************************************************************");

  endfunction


  function string ovm_tr_legal_path_name(string name, string scopeName);
    string legal_name, path_legal_name;

    // Check for a legal stream name. A legal stream name is
    // needed so that debug within Questa works consistently.
    // Questa relies on "legal" names in many places, and
    // having legal names reduces the amount of quoting in tcl scripts.
    // If you are passing in "illegal" names, then recommendation is
    // to change them to legal names.
    // Changing the original name is up to the caller, but since this code is
    // auto-called by OVM code ("aggregate items") it is
    // hard to change the semantics.
    legal_name = ovm_tr_make_legal_c_identifier(name);
    if ( name != legal_name ) begin
      ovm_report_warning("ILLEGALNAME", 
        $psprintf("'%s' is not a legal c identifier name, changed to", name));
      ovm_report_warning("ILLEGALNAME", 
        $psprintf("'%s'. Streams must be named as a legal c identifier.", legal_name));
    end
  
    // Build a name that will make the Questa integration happy.
    // .. means "top" - like the UNIX / in /tmp. There are two dots
    // because of certain historic reasons in the Questa integration.
    // The scopeName is normally (always?) passed in as get_full_name().
    // The lone '.' is a path separator. So the generated path name is:
    //
    //   "..<get_full_name()>.<legal_name>"
    //
    path_legal_name = {"..", scopeName, ".", legal_name};
    return path_legal_name;
  endfunction

// ovm_create_fiber
// ----------------

//Purpose: Given a name, create a stream which transactions
//         are recorded on.
//
// "name" is NOT a path name. It is a simple name.
// "scope" is a full path name, normally the value of
// get_full_name() in the caller.
//
//TODO: What default streams are available, and what
//       are their purposes?
//        'aggregate_items', 'main'
//TODO: What's the use of 't'?
//        TVM, 
//
//TODO: WHy are we using the word fiber?

function STRH ovm_create_fiber (string name,
                                   string t,
                                   string scope);
  STRH h;
  string path_legal_name;

  // Make sure we are initialized, but limit to just
  //  this once, since we always need to call here
  //  before anything meaningful could happen.
  if (!ovm_tr_initialized)
    ovm_tr_init();

  if (OVM_tr_trACE_API)
    $display(">>TR TRACE: %m (%s:%0d)", `__FILE__, `__LINE__);
  if (OVM_tr_trACE_API_DETAIL) begin
    $display("--TR TRACE:         name='%s'",   name);
    $display("--TR TRACE:            t='%s'",      t);
    $display("--TR TRACE:        scope='%s'",  scope);
  end

  path_legal_name = ovm_tr_legal_path_name(name, scope);

  if (!streamhandles.exists(path_legal_name)) begin

    // Only pass a legal name to the $ call.
    if (OVM_tr_trACE_PLI_CALLS)
      $display(">>TR @%0d: $create_transaction_stream(name='%s', t=%s (scope=%s))", 
        $time, path_legal_name, t, scope);
    h = $create_transaction_stream(path_legal_name, t/*Stream Kind*/);

    streamhandles[path_legal_name] = h;
    streamhandles_r[h] = path_legal_name;
    if (OVM_tr_trACE_PLI_CALLS)
      $display(">>TR   created streamh=%0d(%s)", 
        h, NameOfStream(h));
  end
  else begin
    h = streamhandles[path_legal_name];
    if (OVM_tr_trACE_PLI_CALLS)
      $display(">>TR   reusing streamh=%0d(%s)[looked up (%s)]", 
        h, NameOfStream(h), path_legal_name);
  end
  return h;
endfunction

// ovm_set_index_attribute_by_name
// -------------------------------

// TODO: NEVER USED?!?!?!?!

function void NOT_USED_ovm_set_index_attribute_by_name (TRH txh,
                                         string nm,
                                         int index,
                                         logic [1023:0] value,
                                         string radix,
                                         TRH numbits=32);
  $display(">>TR TRACE: %m (%s:%0d)", `__FILE__, `__LINE__);
  $display("--TR TRACE:     txh='%0d'",     txh);
  $display("--TR TRACE:      nm='%s'",       nm);
  $display("--TR TRACE:   index='%0d'",   index);
  $display("--TR TRACE:   value='%b'",    value); // ACK 1024 limit.
  $display("--TR TRACE:   radix='%s'",    radix);
  $display("--TR TRACE: numbits='%0d'", numbits);
  assert(0);
  return;
endfunction


// ovm_set_attribute_by_name
// -------------------------

//Purpose: add a named attribute to this transaction.

//TODO: We need to allow any data type.
//TODO: 1024 bit vector!? That's the wrong datatype.
//TODO: Stick a call to $add_attribute() up way high 
//       in the FIELD AUTOMATION MACROS.
//TODO: radix and numbits -> are they necessary in this API?

function void ovm_set_attribute_by_name (TRH txh,
                                         string nm,
                                         logic [1023:0] value,
                                         string radix,
                                         TRH numbits=0);
  int recorded;
  logic [1023:0] mask;
  string str;	
	
  // UNUSED: radix, numbits.
  //TODO: We can record typed data if had more information
  //      Is this an 'int'? Or a bit vector? Or a logic vector?
  //      Recording 1024 bits is crazy.

//`ifdef EXPERIMENTAL_CODE
//  recorded = 0;
//  //
//  // EXPERIMENTAL CODE TO STOP RECORDING 1024 BITS
//  //
//  if (numbits == 32) begin
//	// Note: probably not right, since 32 could be int, 
//	// unsigned int, bit[31:0], etc, etc, etc.
//	// The call to $add_attribute() should be up high in the
//	// user code, so that the $add_attribute() call is
//	// called with the user data type - then the VPI can
//	// dig around and get the correct type to be recorded.
//	// This is NOT hard - just need a MACRO for the user to invoke.
//	// This macro becomes the "attribute add" API instead
//	// of this function call we are in now.
//    int value_int;
//	value_int = value;
//    $add_attribute(txh, value_int, nm);
//	recorded = 1;
//  end
//  else if (numbits == 1) begin
//    int value_bit;
//    value_bit = value;
//    $add_attribute(txh, value_bit, nm);
//	recorded = 1;
//  end
//  //
//  // END EXPERIMENTAL CODE
//  //
//  if (!recorded)
//`endif // EXPERIMENTAL_CODE
//    $add_attribute(txh, value, nm);
		
	  mask = {1024{1'b1}};
	  mask <<= numbits;
	  mask = ~mask;
	
	  case(radix)
	    "'b":     begin
	               $swrite(str, "%0s%0b", radix, value&mask);
	             end
	    "'o":     begin
	               $swrite(str, "%0s%0o", radix, value&mask);
	             end
	    "'s":     begin
	               if(value[numbits-1] === 1) begin
	                 //sign extend for negative value
	                 logic [1023:0] sval; mask = ~mask; 
	                 sval = (value|mask);
	                 //don't show radix for negative
	                 $swrite(str, "%0d", sval);
	               end
	               else begin
	                 $swrite(str, "'d%0d", (value&mask));
	               end
	             end
	    "'a":  begin
	    					int lastdot;
							  for(lastdot=nm.len()-1; lastdot>0; --lastdot)
							    if(nm[lastdot] == ".") break;
							
							  if(lastdot) begin
							    nm = nm.substr(lastdot+1, nm.len()-1);
							  end	    	
							  
	    					 value[1023:1016] = 8'b0;
	               $swrite(str, "%0s", value);
	             end
	    "'u":    begin
	               $swrite(str, "%0s%0t", radix, value&mask);
	             end
	    default: begin
	               $swrite(str, "%0s%0x", radix, value&mask);
	             end
	  endcase

  if (OVM_tr_trACE_API)
    $display(">>TR TRACE: %m (%s:%0d)", `__FILE__, `__LINE__);
  //TODO - 1024 bit vectors only?
  if (OVM_tr_trACE_PLI_CALLS)
    $display(">>TR @%0d: $add_attribute(txh=%0d(%s), value='%0d', nm=%s, (radix=%s, numbits=%0d))", 
      $time, txh, NameOfTransaction(txh), value, nm, radix, numbits);
    $add_attribute(txh, str, nm);
  return;
endfunction


// ovm_check_handle_kind
// ---------------------

// Purpose: return 1 if the type of the given 'handle' is htype.
//          return 0 otherwise.
// Legal hytpes are 'Stream' and 'Transaction'.

//TODO: Change return type to bit.
//TODO: Change HANDLE type from integer to longint (need/want 64 bits)

function TRH ovm_check_handle_kind (string htype, TRH handle);
  string name;

  // Noisy.
  if (OVM_tr_trACE_API_DETAIL) begin
    $display(">>TR TRACE: %m (%s:%0d)", `__FILE__, `__LINE__);
    $display("--TR TRACE:  htype='%s'",   htype);
    $display("--TR TRACE: handle='%0x'", handle);
  end
  // TODO RETURN true/false. True if this IS such a handle. 
  case (htype)
    "Transaction":
      return transactionhandles_r.exists(handle);
    "Stream":
      return streamhandles_r.exists(handle);
    "Fiber":
      return streamhandles_r.exists(handle);
	default:
	  ovm_report_warning("ILLEGAL HANDLE",
	    $psprintf("\"%s\" is not a known handle type.", htype));
  endcase
  return 0;
endfunction


// ovm_begin_transaction
// ---------------

//Purpose: return a handle to a new transaction.
//         The new transaction has a variety
//         of properties:
//           It is on 'stream'.
//           It has a name.
//           It starts either NOW or at begin_time, if
//             begin_time is non-zero.
//           It has a label.
//           It has a description.

//TODO: You can never create a transaction with begin_time==0.

function TRH ovm_begin_transaction(string txtype,
                                 STRH stream,
                                 string nm, 
                                 string label="", 
                                 string desc="",
                                 time begin_time=0
                                 );
//TR RECORDING: What's the difference between nm, label and description?

  TRH h;
  string path_name;

  if (OVM_tr_trACE_API)
    $display(">>TR TRACE: %m (%s:%0d)", `__FILE__, `__LINE__);
  if (OVM_tr_trACE_API_DETAIL) begin
    $display("--TR TRACE:     txtype='%s'",      txtype);
    $display("--TR TRACE:     stream='%0d'",     stream);
    $display("--TR TRACE:         nm='%s'",          nm);
    $display("--TR TRACE:      label='%s'",       label);
    $display("--TR TRACE:       desc='%s'",        desc);
    $display("--TR TRACE: begin_time='%0d'", begin_time);
  end

  path_name = ovm_tr_legal_path_name(nm, NameOfStream(stream));
  
  // TODO RETURN transaction ID
  if (!transactionhandles.exists(path_name)) begin
    // Only use the time if it is non-zero. (supplied)
    if (begin_time != 0) begin
      if (OVM_tr_trACE_PLI_CALLS)
        $display(">>TR @%0d: $begin_transaction(stream='%0d(%s)', name='%s', begin_time=%0d)", 
          $time, stream, NameOfStream(stream), path_name, begin_time);
      h = $begin_transaction(stream, nm, begin_time);
    end
    else begin
      if (OVM_tr_trACE_PLI_CALLS)
        $display(">>TR @%0d: $begin_transaction(stream='%0d(%s)', name='%s')", 
          $time, stream, NameOfStream(stream), nm);
      h = $begin_transaction(stream, nm);
    end
    transactionhandles[path_name] = h;  //TR RECORDING: This is wrong. There can be many
                                 //              of the same transcation type outstanding.
    transactionhandles_r[h] = path_name;
    if (OVM_tr_trACE_PLI_CALLS)
      $display(">>TR   created tr=%0d(%s)", h, NameOfTransaction(h));
  end
  else begin
    h = transactionhandles[path_name];
    if (OVM_tr_trACE_PLI_CALLS)
      $display(">>TR   reusing tr=%0d(%s)", h, NameOfTransaction(h));
  end

  if (OVM_tr_trACE_API_DETAIL)
    $display("         WLF tr_h='%0d'",  h);

  // UNUSED: txtype, label, desc,
  return h;
endfunction


// ovm_end_transaction
// -------------------

//Purpose: given an open transaction handle, end it.
//         If end_time is non-zero, then end the transaction at
//          end_time.

//TODO: You cannot end a transaction at time 0.

function void ovm_end_transaction (TRH handle, time end_time=0);
  if (OVM_tr_trACE_API)
    $display(">>TR TRACE: %m (%s:%0d)", `__FILE__, `__LINE__);
  if (OVM_tr_trACE_API_DETAIL) begin
    $display("--TR TRACE:   handle='%0x'", handle);
    $display("--TR TRACE: end_time='%0d'", end_time);
  end
  // Only use the time if it is non-zero. (supplied)
  if (end_time != 0) begin
    if (OVM_tr_trACE_PLI_CALLS)
      $display(">>TR @%0d: $end_transaction(handle=%0x(%s), end_time=%0d)", 
        $time, handle, NameOfTransaction(handle), end_time);
    $end_transaction(handle, end_time);
  end
  else begin
    if (OVM_tr_trACE_PLI_CALLS)
      $display(">>TR @%0d: $end_transaction(handle=%0x(%s))", 
        $time, handle, NameOfTransaction(handle));
    $end_transaction(handle);
  end
  return;
endfunction


// ovm_link_transaction
// --------------------

//Purpose: given two transction handles, 
//           create a "relationship" between them.

//TODO: What about built-in relationships?

function void ovm_link_transaction(TRH h1, TRH h2,
                                   string relation="");
  if (OVM_tr_trACE_API)
    $display(">>TR TRACE: %m (%s:%0d)", `__FILE__, `__LINE__);
  if (OVM_tr_trACE_API_DETAIL) begin
    $display("--TR TRACE:              h1='%0d'", h1);
    $display("--TR TRACE:              h2='%0d'", h2);
    $display("--TR TRACE: before relation='%s'", relation);
  end
  if (relation == "")
    relation = "successor";
  if (OVM_tr_trACE_API_DETAIL) begin
    $display("--TR TRACE:  after relation='%s'", relation);
  end
  if (OVM_tr_trACE_PLI_CALLS)
    $display(">>TR @%0d: $add_relation(h1=%0d(%s), h2=%0d(%s), relation='%s')", 
      $time, h1, NameOfTransaction(h1), h2, NameOfTransaction(h2), relation);
  $add_relation(h1, h2, relation);
  return;
endfunction



// ovm_free_transaction_handle
// ---------------------------

//Purpose: given a transaction handle, release storage for it.
//         Calling free_transaction_handle() means that the
//         handle is no longer to be used anywhere. After this
//         call the transaction handle is invalid.

function void ovm_free_transaction_handle(TRH handle);
  if (OVM_tr_trACE_API)
    $display(">>TR TRACE: %m (%s:%0d)", `__FILE__, `__LINE__);
  if (OVM_tr_trACE_API_DETAIL) begin
    $display("           handle='%0x'", handle);
    $display("$free_transaction(0x%x=%0d);", handle, handle);
  end
  if (transactionhandles_r.exists(handle)) begin
    string name;
    if (OVM_tr_trACE_PLI_CALLS)
      $display(">>TR @%0d: $free_transaction(handle=%0x(%s))", 
        $time, handle, NameOfTransaction(handle));
    $free_transaction(handle);
    name = transactionhandles_r[handle];
    transactionhandles_r.delete(handle);
    transactionhandles.delete(name);
  end
  return;
endfunction

`endif // OVM_RECORD_INTERFACE


