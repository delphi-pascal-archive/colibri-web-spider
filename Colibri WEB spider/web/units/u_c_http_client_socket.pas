// 003 u_c_http_client_socket
// 06 jun 2005

// -- (C) Felix John COLIBRI 2004
// -- documentation: http://www.felix-colibri.com

(*$r+*)

unit u_c_http_client_socket;
  interface
    uses u_c_client_socket_4
         ;

    const k_http_port= 80;     
    type c_http_client_socket= Class; // forward
         t_po_http_event= procedure(p_c_http_client_socket: c_http_client_socket) of Object;

         c_http_client_socket= class(c_client_socket)
                                 m_get_request: String;

                                 // -- ""
                                 m_requested_page: String;

                                 // -- for debug display. "200 ..."
                                 m_end_of_header_index: Integer;
                                 // -- from the answer header
                                 m_content_length: Integer;
                                 // -- how much was received beyond the header
                                 m_downloaded_content_length: Integer;

                                 m_on_http_connected: t_po_http_event;
                                 m_on_http_received_data: t_po_http_event;
                                 m_on_after_received_http_body: t_po_http_event;

                                 Constructor create_http_client_socket(p_name: String);

                                 procedure lookup_connect_get(p_server_name: String;
                                    p_port: Integer; p_get_request: String);
                                 procedure connect_get(p_ip_address: String;
                                    p_port: Integer; p_get_request: String);

                                 procedure connect_server_name(p_server_name: String; p_port: Integer);
                                 procedure _handle_after_lookup(p_c_client_socket: c_client_socket);
                                 procedure connect_ip(p_ip_address: String; p_port: Integer);

                                 procedure _handle_http_connected(p_c_client_socket: c_client_socket);
                                 procedure _handle_received_data(p_c_client_socket: c_client_socket);

                                 Destructor Destroy; Override;
                               end; // c_http_client_socket

  implementation
    uses SysUtils, u_c_display, u_strings;

    // -- c_http_client_socket

    Constructor c_http_client_socket.create_http_client_socket(p_name: String);
      begin
        Inherited create_client_socket(p_name);

        m_on_after_socket_connected:= _handle_http_connected;
        m_on_after_socket_received_data:= _handle_received_data;
        // -- after server closed initialized by the user
      end; // create_http_client_socket

    procedure c_http_client_socket.lookup_connect_get(p_server_name: String;
        p_port: Integer; p_get_request: String);
      begin
        trace_socket('> lookup_connect_get');

        m_get_request:= p_get_request;

        connect_server_name(p_server_name, p_port);

        trace_socket('< lookup_connect_get');
      end; // lookup_connect_get

    procedure c_http_client_socket.connect_get(p_ip_address: String;
        p_port: Integer; p_get_request: String);
      begin
        trace_socket('> connect_get');
        m_get_request:= p_get_request;
        wsa_startup;
        connect_ip(p_ip_address, p_port);
        trace_socket('< connect_get');
      end; // connect_get

    procedure c_http_client_socket.connect_server_name(p_server_name: String; p_port: Integer);
        // -- lookup+ connect
      begin
        trace_socket('> connect_server_name');
        m_on_after_socket_host_lookup:= _handle_after_lookup;
        m_remote_port:= p_port;

        wsa_startup;
        do_lookup_win_socket_server(p_server_name);
        trace_socket('< connect_server_name');
      end; // connect_server_name

    procedure c_http_client_socket._handle_after_lookup(p_c_client_socket: c_client_socket);
      begin
        trace_socket('> c_http_client._after_lookup');
        connect_ip(m_remote_ip, m_remote_port);
        trace_socket('< c_http_client._after_lookup');
      end; // _handle_after_lookup

    procedure c_http_client_socket.connect_ip(p_ip_address: String; p_port: Integer);
      begin
        trace_socket('> c_http_client.connect_ip');
        create_win_socket;
        wsa_select;
        do_connect_win_socket(p_ip_address, p_port);
        trace_socket('< c_http_client.connect_ip');
      end; // connect

    procedure c_http_client_socket._handle_http_connected(p_c_client_socket: c_client_socket);
        // -- simple relay
      begin
        trace_socket('> c_http_client._connected');
        f_do_send_string(m_get_request);
        if Assigned(m_on_http_connected)
          then m_on_http_connected(Self);
        trace_socket('< c_http_client._connected');
      end; // _handle_http_connected

    procedure c_http_client_socket._handle_received_data(p_c_client_socket: c_client_socket);
        // -- fd_read notification was received

      procedure extract_answer_code;
          // -- "HTTP/1.1 404 Not Found"
        var l_index: Integer;
            l_protocol, l_answer_code: String;
            l_error_index: Integer;
        begin
          l_index:= 1;
          // -- skip first
          l_protocol:= f_string_extract_non_blank(m_answer_line, l_index);
          l_answer_code:= f_string_extract_non_blank(m_answer_line, l_index);
          // stop(m_answer_line+ '|'+ l_answer_code);
          Val(l_answer_code, m_answer_code, l_error_index);
        end; // extract_answer_code

      var l_content_length_position: Integer;
          l_content_length_string: String;

      begin // _handle_received_data
        trace_socket('> c_http_client._received');

        if Assigned(m_on_http_received_data)
          then m_on_http_received_data(Self);

        with m_c_reception_buffer do
        begin
          if m_end_of_header_index= 0
            then m_end_of_header_index:= f_next_2_new_line_position(0);

          if m_end_of_header_index> 0
            then begin
                m_downloaded_content_length:= m_write_index- (m_end_of_header_index+ 4);

                if f_extract_answer_line and (m_answer_code< 0)
                  then extract_answer_code;

                display('%'+ f_extract_string_start_end(m_display_index, m_write_index- 1)+ '%');
                m_display_index:= m_write_index- 1;

                l_content_length_position:= f_string_position(0, 'Content-Length');
                // display('content_length_position '+ IntToStr(l_content_length_position));
                if l_content_length_position> 0
                  then begin
                      Inc(l_content_length_position, Length('Content-Length')+ 1);
                      l_content_length_string:= f_extract_eol(l_content_length_position);
                      display('content_length >'+ l_content_length_string+ '<');

                      if l_content_length_string<> ''
                        then begin
                            m_content_length:= StrToInt(l_content_length_string);
                            m_read_index:= m_end_of_header_index+ 4;

                            if m_write_index- m_read_index= m_content_length
                              then begin
                                  // m_answer:= f_extract_string_start_end(m_read_index, m_write_index- 1);
                                  if Assigned(m_on_after_received_http_body)
                                    then m_on_after_received_http_body(Self);
                                end
                              else display(IntToStr(m_write_index- m_read_index)+ ' <> '+ IntToStr(m_content_length));
                          end;
                    end;      
              end;
        end; // with m_c_reception_buffer

        trace_socket('< c_http_client._received');
      end; // _handle_received_data

    // -- free

    Destructor c_http_client_socket.Destroy;
      begin
        Inherited;
      end; // Destroy

    begin // u_c_http_client_socket
    end. // u_c_http_client_socket


