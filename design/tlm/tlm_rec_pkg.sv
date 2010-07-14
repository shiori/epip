
package tlm_rec_pkg;
    typedef class tlm_rec_stream;
    typedef class tlm_rec_trans;

    class tlm_rec_trans;
        local int handle, stream_handle;
        local tlm_rec_trans child, parent;
        
        function new(int n_handle);
            assert(n_handle);
            handle = 0;
            stream_handle = n_handle;
            child = null;
            parent = null;
        endfunction

        function void add_field(string nm, cont);
            assert(handle);
            
            $add_attribute(handle, cont, nm);
        endfunction
                        
        function void tr_end();
            if(child != null)
                child.tr_end();
            
            $end_transaction(handle);
            $free_transaction(handle);
            handle = 0;
        endfunction
        
        function void tr_begin(string nm, time t);
            assert(handle == 0);
            handle = $begin_transaction(stream_handle, nm, t);
            if(parent != null)
                $add_relation(parent.handle, handle, "child" );
        endfunction

        function tlm_rec_trans creat_child();
            tlm_rec_trans t;
            assert(handle);
            
            t = new(stream_handle);
            t.parent = this;
            child = t;
            return t;
        endfunction
                
    endclass : tlm_rec_trans
        
    class tlm_rec_stream;
        local int handle;
        
        function void create_stream(string n = "", pn = "", rn = "/ovm_root/", sp = "/", rp = ".");
            string nm;
            int i;
            for (i = 0; i <  pn.len(); i=i+1)
                if ( pn[i] == rp)
                     pn[i] = sp;
                     
            if(pn == "")
                nm = n;
            else
                nm = $psprintf("%s%s%s%s", rn, pn, sp, n);
                
            $display(nm);
            handle = $create_transaction_stream(nm);
        endfunction

        function tlm_rec_trans create_trans();
            tlm_rec_trans t;
            assert(handle);
            
            t = new(handle);
            return t;
        endfunction
                
        function new();
            handle = 0;
        endfunction
                
    endclass : tlm_rec_stream

endpackage : tlm_rec_pkg
