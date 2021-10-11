// 003 u_c_base_socket_4
// 20 may 2005

// -- (C) Felix John COLIBRI 2004
// -- documentation: http://www.felix-colibri.com

// -- common to client and server

(*$r+*)

unit u_c_base_socket_4;
  interface
    uses Windows, Messages, WinSock
      , SysUtils // exception
      , u_c_basic_object
      ;

    type t_socket_error= (e_no_socket_error
             , e_wsa_startup_error
             , e_create_socket_error
             , e_wsa_select_error
             , e_connect_error
             , e_wsa_lookup_host_error
             , e_lookup_result_error, e_notification_error
             , e_receive_data_error, e_send_data_error
             , e_wsa_cancel_lookup_error, e_close_socket_error
             , e_win_proc_exception);

         c_socket_exception= Class(Exception)
                               constructor create_socket_exception(p_socket_handle: Integer; p_text: String;
                                 p_socket_error: Integer);
                             end;

         // -- the socket messages
         t_wm_async_select= record
                             Msg: Cardinal;
                             m_socket_handle: TSocket;
                             // -- lParam.Lo
                             m_select_event: Word;
                             // -- lParam.Hi
                             m_select_error: Word;
                             Result: Longint;
                           end;

         c_base_socket= Class;
         // -- triggered when received data
         t_po_base_socket_event= Procedure(p_c_base_socket: c_base_socket) of Object;
         t_po_traffic_event= Procedure(p_traffic: String) of Object;

         c_base_socket= class(c_basic_object)
                          m_trace_socket: Boolean;

                          // -- WsaData "mailbox variable" to display the data
                          m_wsa_data: twsaData;

                          // -- "mailbox variables"
                          m_remote_ip: String;
                          m_remote_port: Integer;

                          m_local_ip: String;
                          m_local_port: Integer;

                          m_base_socket_handle: tSocket;
                          m_window_handle: hWnd;

                          // -- if handle is used as a key, must save the
                          m_save_socket_handle: tSocket;

                          // -- called Listen or Connect
                          m_is_open: Boolean;

                          m_socket_error: t_socket_error;
                          m_socket_error_text: String;
                          m_wsa_error_code: Integer;
                          m_wsa_socket_error_message: String;

                          // -- notify the user about an exception
                          m_on_after_socket_error: t_po_base_socket_event;

                          // -- somebody called "do_close_socket" (also called by server closed, and Free)
                          // m_on_before_do_close_socket,
                          m_on_after_do_close_socket: t_po_base_socket_event;
                          m_on_display_traffic: t_po_traffic_event;

                          Constructor create_base_socket(p_name: String);

                          function f_display_base_socket: String;
                          function f_name_and_handle(p_handle: Integer): String;
                          procedure trace_socket(p_text: String);

                          procedure raise_exception(p_socket_error: t_socket_error;
                              p_wsa_error_code: Integer; p_error_text: String);

                          procedure wsa_startup;
                          procedure display_wsa_data;
                          procedure allocate_window;
                          procedure create_win_socket;

                          procedure WndProc(var Message: TMessage);

                          function f_format_traffic(p_direction: Char; p_traffic: String): String;

                          function f_do_send_string(p_string: String): Integer; Virtual;
                          function f_do_send_buffer(p_pt_buffer: Pointer;
                              p_send_count: Integer): Integer; Virtual;
                          function f_do_read_data(p_pt: Pointer; p_get_max: Integer): Integer;

                          procedure do_close_win_socket; Virtual;

                          Destructor Destroy; Override;
                        end; // c_base_socket


  implementation
    uses // SysUtils
       Forms // Application
      , u_characters 
      , u_c_display, u_display_hex_2, u_strings
      , u_winsock
      ;

    constructor c_socket_exception.create_socket_exception(p_socket_handle: Integer; p_text: String;
        p_socket_error: Integer);
      begin
        InHerited Create('Handle '+ IntToStr(p_socket_handle)
            + ' err= '+ p_text+ ' sock_err '+ IntToStr(p_socket_error));
      end; // create_socket_exception

    // -- c_base_socket

    Constructor c_base_socket.create_base_socket(p_name: String);
      begin
        Inherited create_basic_object(p_name);
        m_base_socket_handle:= INVALID_SOCKET;
      end; // create_base_socket

    function c_base_socket.f_name_and_handle(p_handle: Integer): String;
      begin
        Result:= m_name+ '_'+ f_socket_handle_string(p_handle);
      end; // f_name_and_handle

    function c_base_socket.f_display_base_socket: String;
        // -- debug
      begin
        Result:= f_name_and_handle(m_base_socket_handle)
            + ' $'+ f_integer_to_hex(Integer(Self));
      end; // f_display_base_socket

    procedure c_base_socket.trace_socket(p_text: String);
      begin
        if m_trace_socket
          then display(p_text);
      end; // trace_socket

    procedure c_base_socket.raise_exception(p_socket_error: t_socket_error;
        p_wsa_error_code: Integer; p_error_text: String);
      begin
        if assigned(m_on_after_socket_error)
          then begin
              m_socket_error:= p_socket_error;
              m_wsa_error_code:= p_wsa_error_code;
              m_socket_error_text:= p_error_text;
              m_wsa_socket_error_message:= f_socket_error_message(m_wsa_error_code);
              m_on_after_socket_error(Self);
            end
          else Raise c_socket_exception.create_socket_exception(m_base_socket_handle, p_error_text, p_wsa_error_code);
      end; // raise_exception

    // -- the usual wsa calls

    procedure c_base_socket.wsa_startup;
      var l_version: Word;
          l_result: Integer;
      begin
        trace_socket('');
        trace_socket('> wsa_startup');
        // -- l_version:= $0202;
        l_version:= $0101;
        trace_socket(':wsaStartup()');
        l_result:= wsaStartup(l_version, m_wsa_data);
        if l_result<> 0
          then raise_exception(e_wsa_startup_error, WSAGetLastError, 'WsaStartup');

        trace_socket('< wsa_startup');
      end; // wsa_startup

    procedure c_base_socket.display_wsa_data;
      begin
        with m_wsa_data do
        begin
          display('wVersion '+ IntToHex(wVersion, 4));
          display('wHighVersion '+ IntToHex(wHighVersion, 4));
          display('szDescription '+ string(szDescription));
          display('szSystemStatus '+ string(szSystemStatus));
          display('iMaxSockets '+ IntToStr(iMaxSockets));
          display('iMaxUdpDg '+ IntToStr(iMaxUdpDg));
          if lpVendorInfo= nil
            then display('lpVendorInfo NIL');
        end;
      end; // display_wsa_data

    procedure c_base_socket.create_win_socket;
      begin
        trace_socket('');
        trace_socket('> create_win_socket');

        // -- allocate the Socket record, and get back its handle
        trace_socket(':Socket(pf_Inet, Sock_Stream)');
        m_base_socket_handle:= Socket(PF_INET, SOCK_STREAM, IPPROTO_IP);
        if m_base_socket_handle= INVALID_SOCKET
          then raise_exception(e_create_socket_error, WSAGetLastError, 'Socket()');

        // -- local addr can be undefined because did not Connect yet
        trace_socket('< create_win_socket '+ f_name_and_handle(m_base_socket_handle));
      end; // create_win_socket

    procedure c_base_socket.allocate_window;
      begin
        if m_window_handle= 0
          then begin
              m_window_handle:= AllocateHwnd(WndProc);
              trace_socket('allocate_window '+ IntToStr(m_window_handle));
            end;
      end; // allocate_window

    procedure c_base_socket.WndProc(var Message: TMessage);
      begin
        try
          Dispatch(Message);
        except
          on e: exception do
            begin
              display('*** c_base_socket.WndProc_exception '+ e.Message);
              raise_exception(e_win_proc_exception, 0, 'c_base_socket.WndProc_exception '+ e.Message);
            end;
        end; // try except
      end; // WndProc

    // -- received some data

    function c_base_socket.f_format_traffic(p_direction: Char; p_traffic: String): String;
      var l_prefix: String;
          l_index: Integer;
      begin
        l_prefix:= Format('%5d ', [m_base_socket_handle])+ p_direction+ ' ';
        Result:= l_prefix;
        l_index:= 1;
        while l_index<= Length(p_traffic) do
        begin
          if p_traffic[l_index]= k_return
            then begin
                Result:= Result+ k_new_line;
                Inc(l_index, 1);
                if l_index+ 1< Length(p_traffic)
                  then Result:= Result+ l_prefix;
              end
            else Result:= Result+ p_traffic[l_index];
          Inc(l_index);
        end; // while l_index
      end; // f_format_traffic

    function c_base_socket.f_do_read_data(p_pt: Pointer; p_get_max: Integer): Integer;
        // -- get the data from the client socket
      var l_display_traffic: String;
      begin
        trace_socket('');
        trace_socket('> f_do_read_data '+ f_name_and_handle(m_base_socket_handle));

        trace_socket(':Recv('+ f_socket_handle_string(m_base_socket_handle)+ ', pt^, '
            + IntToStr(p_get_max)+ ', 0)');
        Result:= Recv(m_base_socket_handle, p_pt^, p_get_max, 0);

        if (Result> 0) and Assigned(m_on_display_traffic)
          then begin
              SetLength(l_display_traffic, Result);
              Move(p_pt^, l_display_traffic[1], Result);
              m_on_display_traffic(f_format_traffic('<', l_display_traffic));
            end;

        if Result< 0
          then raise_exception(e_receive_data_error, WSAGetLastError, 'f_receive_data - Recv');

        trace_socket('< f_do_read_data. Received ='+ IntToStr(Result));
      end; // f_do_read_data

    // -- user sends data

    function c_base_socket.f_do_send_buffer(p_pt_buffer: Pointer;
        p_send_count: Integer): Integer;
      var l_display_traffic: String;
      begin
        trace_socket('');
        trace_socket('> f_do_send_buffer '+ f_name_and_handle(m_base_socket_handle));

        trace_socket(':Send('+ f_socket_handle_string(m_base_socket_handle)+ ', pt^, '
            + IntToStr(p_send_count)+ ', 0)');
        Result:= Send(m_base_socket_handle, p_pt_buffer^, p_send_count, 0);
        if Result< 0
          then raise_exception(e_send_data_error, WSAGetLastError, 'send_buffer Send')
          else begin
              SetLength(l_display_traffic, Result);
              Move(p_pt_buffer^, l_display_traffic[1], Result);
              m_on_display_traffic(f_format_traffic('>', l_display_traffic));
            end;

        trace_socket('< f_do_send_buffer. Sent='+ IntToStr(Result));
      end; // send_buffer

    function c_base_socket.f_do_send_string(p_string: String): Integer;
      begin
        trace_socket('');
        trace_socket('> f_send_string '+ f_name_and_handle(m_base_socket_handle));

        trace_socket(':Send('+ f_socket_handle_string(m_base_socket_handle)+ ', '
            + f_display_string(p_string)+ ', '+ IntToStr(Length(p_string))+ ', 0)');
        Result:= Send(m_base_socket_handle, p_string[1], Length(p_string), 0);
        if Result< 0
          then raise_exception(e_send_data_error, WSAGetLastError, 'send_string Send')
          else
            if Assigned(m_on_display_traffic)
              then m_on_display_traffic(f_format_traffic('>', p_string));

        trace_socket('< f_do_send_string. Sent = '+ IntToStr(Result));
      end; // f_do_send_string

    procedure c_base_socket.do_close_win_socket;
      var l_result: Integer;
          l_trace_socket: boolean;
      begin
        trace_socket('');
        trace_socket('> do_close_win_socket '+ f_name_and_handle(m_base_socket_handle));

        m_save_socket_handle:= m_base_socket_handle;
        l_trace_socket:= m_trace_socket;

        if m_base_socket_handle= INVALID_SOCKET
          then trace_socket('INVALID_SOCKET (already_closed ?)')
          else begin
              trace_socket(':CloseSocket('+ f_socket_handle_string(m_base_socket_handle)+ ')');
              l_result:= CloseSocket(m_base_socket_handle);
              if l_result<> 0
                then raise_exception(e_close_socket_error, WSAGetLastError, 'CloseSocket')
                else begin
                    m_base_socket_handle:= INVALID_SOCKET;
                    m_is_open:= False;
                  end;
            end;

        if Assigned(m_on_after_do_close_socket)
          then m_on_after_do_close_socket(Self);

        if l_trace_socket
          then display('< do_close_win_socket');
      end; // do_close_win_socket

    Destructor c_base_socket.Destroy;
      begin
        trace_socket('> c_base_socket.Destroy');

        if m_is_open and (m_base_socket_handle<> INVALID_SOCKET)
          then do_close_win_socket;

        Inherited;

        trace_socket('< c_base_socket.Destroy');
      end; // Destroy

    begin // u_c_base_socket
    end. // u_c_base_socket


