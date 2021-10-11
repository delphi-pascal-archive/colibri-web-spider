// 003 u_c_base_socket_with_buffer_4
// 06 jun 2005

// -- (C) Felix John COLIBRI 2004
// -- documentation: http://www.felix-colibri.com

// -- c_base_socket plus
// --  - c_byte_buffer of received / sent bytes (fetch, stats, save)
// --  - c_log received / sent data

(*$r+*)

unit u_c_base_socket_with_buffer_4;
  interface
    uses Classes, u_c_basic_object
        , u_c_log
        , u_c_base_socket_4
        , u_c_byte_buffer
        ;

    const // -- fixed initial size, reallocated dynamically
          k_buffer_max= 1024* 16;
          k_buffer_slack_min= 1024* 2;
          k_tcp_ip_chunk= 2024;

          k_unknown_answer_line= 'fc_??';

    type c_base_socket_with_buffer= Class; // forward
         t_po_client_socket_with_buffer_event= procedure(p_c_base_socket_with_buffer: c_base_socket_with_buffer) of object;

         c_base_socket_with_buffer= class(c_base_socket)
                                        m_c_traffic_log_ref: c_log;
                                        m_do_free_traffic_log: Boolean;

                                        // -- the reception buffer
                                        m_c_reception_buffer: c_byte_buffer;
                                        m_c_emission_buffer: c_byte_buffer;

                                        // -- only for stats
                                        m_packet_bytes: Integer;

                                        m_answer_code: Integer;
                                        m_answer_line: String;

                                        Constructor create_base_socket_with_buffer(p_name: String);
                                        function f_c_self: c_base_socket_with_buffer;

                                        procedure create_traffic_log(p_full_file_name: String);
                                        procedure write_to_log(p_text: String);

                                        procedure reset_answer;
                                        function f_do_send_string(p_string: String): Integer; Override;
                                        function f_do_send_buffer(p_pt_buffer: Pointer;
                                            p_buffer_max: Integer): Integer; Override;

                                        procedure display_reception_stats;
                                        procedure send_buffered_data; Virtual;
                                        procedure receive_buffered_data; Virtual;

                                        // -- extract 200
                                        function f_extract_answer_line: Boolean;
                                        function f_received_answer_line: Boolean;
                                        function f_received_return_dot_return: Boolean;
                                        procedure skip_line;

                                        Destructor Destroy; Override;
                                      end; // c_base_socket_with_buffer

  implementation
    uses SysUtils, TypInfo, u_c_display, u_types_constants, u_characters
       , u_strings, u_file
       ;

    // -- c_base_socket_with_buffer

    Constructor c_base_socket_with_buffer.create_base_socket_with_buffer(p_name: String);
      begin
        Inherited create_base_socket(p_name);

        m_c_reception_buffer:= c_byte_buffer.create_byte_buffer('read_buffer', k_buffer_max);
        m_c_emission_buffer:= c_byte_buffer.create_byte_buffer('write_buffer', k_buffer_max);
      end; // create_base_socket_with_buffer

    procedure c_base_socket_with_buffer.create_traffic_log(p_full_file_name: String);
      begin
        m_do_free_traffic_log:= True;
        m_c_traffic_log_ref:= c_log.create_log('traffic', p_full_file_name);
      end; // create_traffic_log

    procedure c_base_socket_with_buffer.reset_answer;
      begin
        m_answer_line:= k_unknown_answer_line;
        m_answer_code:= -2;
      end; // reset_answer

    function c_base_socket_with_buffer.f_do_send_string(p_string: String): Integer;
      begin
        reset_answer;
        Result:= Inherited f_do_send_string(p_string);
      end; // f_do_send_string

    function c_base_socket_with_buffer.f_do_send_buffer(p_pt_buffer: Pointer;
        p_buffer_max: Integer): Integer;
      begin
        reset_answer;
        Result:= Inherited f_do_send_buffer(p_pt_buffer, p_buffer_max);
      end; // f_do_send_buffer

    procedure c_base_socket_with_buffer.write_to_log(p_text: String);
      begin
        if m_c_traffic_log_ref<> nil
          then m_c_traffic_log_ref.write_line(p_text);
      end; // write_to_log

    function c_base_socket_with_buffer.f_c_self: c_base_socket_with_buffer;
      begin
        Result:= Self;
      end; // f_c_self

    procedure c_base_socket_with_buffer.display_reception_stats;
      begin
      end; // display_reception_stats

    procedure c_base_socket_with_buffer.send_buffered_data;
        // -- fd_write notification was received
        // --  - send whatever is in the buffer
      begin
        // -- todo
        // -- and log
      end; // send_buffered_data

    procedure c_base_socket_with_buffer.receive_buffered_data;
        // -- fd_read notification was received
        // --  - analyze the different answers to connect, user, pass etc
      var l_remaining: Integer;
          l_pt_start_reception: Pointer;
          l_received_text: String;
      begin
        with m_c_reception_buffer do
        begin
          l_remaining:= m_buffer_size- m_write_index;

          // -- if not at least a tcp-ip chunk, increase the room
          if l_remaining< k_tcp_ip_chunk
            then begin
                // -- reallocate
                // -- ds: should put in critical section ?
                double_the_capacity;
                l_remaining:= m_buffer_size- m_write_index;
              end;

          // -- add the received data to the current buffer
          l_pt_start_reception:= @ m_oa_byte_buffer[m_write_index];

          // -- get the data from the client socket
          m_packet_bytes:= f_do_read_data(l_pt_start_reception, l_remaining);

          if m_packet_bytes> 0
            then begin
                m_write_index:= m_write_index+ m_packet_bytes;

                if m_c_traffic_log_ref<> Nil
                  then begin
                      l_received_text:= f_extract_string_start_end(m_write_index- m_packet_bytes, m_write_index- 1);
                      write_to_log('R '+ l_received_text);
                    end;
              end; // if l_packet_bytes
        end; // with m_c_reception_buffer
      end; // receive_buffered_data

    // -- different extractions

    function c_base_socket_with_buffer.f_extract_answer_line: Boolean;
      var l_return_line_feed_position: Integer;
      begin
        with m_c_reception_buffer do
        begin
          Result:= True;
          if (m_answer_line= '') or (m_answer_line= k_unknown_answer_line)
            then begin
                l_return_line_feed_position:= f_return_line_feed_position(m_read_index);
                if l_return_line_feed_position<> -1
                  then m_answer_line:= f_extract_string_start_end(m_read_index, l_return_line_feed_position- 1)
                  else Result:= False;
              end;
        end; // with m_c_reception_buffer
      end; // f_extract_answer_line

    function c_base_socket_with_buffer.f_received_answer_line: Boolean;
        // -- true if received Return-Line_feed
        // -- result in m_answer_code (and m_answer_line for display)
      var l_return_line_feed_position: Integer;

      function f_extract_code: Integer;
          // -- has received a full line. Extract the code
          // -- is in first 3 characters
        var l_code_string: String;
            l_error_index: Integer;
        begin
          // -- the code is in the first 3 characters of the line
          with m_c_reception_buffer do
            l_code_string:= f_extract_string_start_end(m_read_index, m_read_index+ 2);
          Val(l_code_string, Result, l_error_index);

          with m_c_reception_buffer do
            m_answer_line:= f_extract_string_start_end(m_read_index, l_return_line_feed_position- 3);
        end; // f_extract_code

      begin // f_received_answer_line
        with m_c_reception_buffer do
        begin
          l_return_line_feed_position:= f_return_line_feed_position(m_read_index);

          if l_return_line_feed_position<> -1
            then begin
                m_answer_code:= f_extract_code;
                Result:= True;
              end
            else begin
                m_answer_line:= '';
                m_answer_code:= -1;
                Result:= False;
              end;
        end; // with m_c_reception_buffer
      end; // f_received_answer_line

    function c_base_socket_with_buffer.f_received_return_dot_return: Boolean;
      begin
        with m_c_reception_buffer do
          Result:= f_pos(k_new_line+ '.'+ k_new_line)>= 0;
      end; // f_received_return_dot_return

    procedure c_base_socket_with_buffer.skip_line;
        // -- move read index beyond the current line
      begin
        with m_c_reception_buffer do
          f_skip_return_line_feed(m_read_index);
      end; // skip_line

    Destructor c_base_socket_with_buffer.Destroy;
      begin
        m_c_reception_buffer.Free;
        m_c_emission_buffer.Free;

        if m_do_free_traffic_log
          then m_c_traffic_log_ref.Free;

        Inherited;
      end; // Destroy

    begin // u_c_ftp_client
    end. // u_c_ftp_client


