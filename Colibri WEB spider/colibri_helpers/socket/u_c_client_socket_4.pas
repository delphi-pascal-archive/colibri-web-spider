// 005 u_c_client_socket_4
// 04 jun 2005

// -- (C) Felix John COLIBRI 2004
// -- documentation: http://www.felix-colibri.com

// -- lookup
// -- wsaStartup, Socket, wsaSelect, Connect, Send
// --   (no Accept, Bind, Listen)

(*$r+*)

unit u_c_client_socket_4;
  interface
    uses Windows, Messages, WinSock
      , SysUtils // exception
      , u_c_basic_object
      , u_c_base_socket_4
      , u_c_base_socket_with_buffer_4
      ;

    const wm_asynch_select= wm_User;
          wm_lookup_host= wm_User+ $0001;
          // wm_defer_free= wm_User+ $0002;

    type t_wm_lookup_host= record
                              Msg: Cardinal;
                              // -- wParam
                              m_lookup_handle: THandle;
                              // -- lParam.Lo
                              AsyncBufLen: Word;
                              // -- lParam.Hi
                              m_lookup_error: Word;
                              // -- result to return to WinProc
                              Result: Longint;
                            end;

         c_client_socket= Class;
         // -- triggered when received data
         t_po_client_socket_event= Procedure(p_c_client_socket: c_client_socket) of Object;

         c_client_socket= class(c_base_socket_with_buffer)
                            // -- mailbox
                            m_server_name: String;
                            // -- lookup the server-name 
                            m_lookup_handle: THandle;
                            m_pt_get_host_data: Pointer;

                            // -- some descendent workload
                            m_c_object: tObject;

                            m_do_free_after_server_closed: Boolean;

                            m_on_after_socket_host_lookup: t_po_client_socket_event;
                            // -- the connection was established
                            m_on_after_socket_connected: t_po_client_socket_event;

                            // -- has sent whatever was in the m_c_emission_buffer
                            m_on_after_socket_wrote_data: t_po_client_socket_event;
                            // -- some data arrived
                            m_on_after_socket_received_data: t_po_client_socket_event;

                            // -- the server_client_socket closed the connection (all sent ?)
                            m_on_after_remote_server_client_socket_closed: t_po_client_socket_event;

                            Constructor create_client_socket(p_name: String);

                            procedure wsa_select; Virtual;

                              procedure handle_wm_lookup_host(var pv_wm_lookup_host:
                                     t_wm_lookup_host); Message wm_lookup_host;
                            procedure do_lookup_win_socket_server(p_server_name: String);

                            procedure do_connect_win_socket(p_server_ip: String; p_port: Integer);

                            procedure handle_wm_async_select(var pv_wm_async_select:
                                t_wm_async_select); Message wm_asynch_select;
(*
                            procedure handle_wm_defer_free(Var Message);
                                Message wm_defer_free;
*)
                            procedure do_close_win_socket; Override;

                            Destructor Destroy; Override;
                          end; // c_client_socket

  implementation
    uses // SysUtils
       Forms // Application

      , u_c_display, u_strings
      , u_winsock
      ;

    // -- c_client_socket

    Constructor c_client_socket.create_client_socket(p_name: String);
      begin
        Inherited create_base_socket_with_buffer(p_name);
      end; // create_client_socket

    procedure c_client_socket.wsa_select;
      var l_result: Integer;
      begin
        trace_socket('');
        trace_socket('> wsa_select '+ f_name_and_handle(m_base_socket_handle));

        allocate_window;

        // -- specifies which WinSocket events should be monitored
        // -- add WRITE (if not, connect can trigger WSA_E_WOULDBLOCK)
        trace_socket(':wsaAsyncSelect('+ f_socket_handle_string(m_base_socket_handle)+
            ', CONNECT+ READ+ WRITE+ CLOSE, )');
        l_result:= wsaAsyncSelect(m_base_socket_handle, m_window_handle,
            wm_asynch_select,
            FD_CONNECT+ FD_READ+ FD_WRITE+ FD_CLOSE);
        if l_result<> 0
          then raise_exception(e_wsa_select_error, WSAGetLastError, 'wsaAsyncSelect');

        trace_socket('< wsa_select');
      end; // wsa_select

    procedure c_client_socket.do_lookup_win_socket_server(p_server_name: String);
        // -- only knows the server's domainname "www.borland.com"
        // -- must first lookup the server IP
      begin
        trace_socket('> do_lookup_win_socket_server '+ p_server_name);

        m_server_name:= p_server_name;

        // -- allocate the structure for the returned IP address
        if m_pt_get_host_data= nil
          then m_pt_get_host_data:= AllocMem(MAXGETHOSTSTRUCT);

        allocate_window;

        trace_socket(':WSAAsyncGetHostByName(whn='+ IntToStr(m_window_handle));
        m_lookup_handle:= WSAAsyncGetHostByName(m_window_handle, wm_lookup_host,
            PChar(p_server_name),
            m_pt_get_host_data, MAXGETHOSTSTRUCT);

        if m_lookup_handle= 0
          then begin
              display('*** async_lookup_handle '+ p_server_name+ '<');
              raise_exception(e_wsa_lookup_host_error, 0, 'WSAAsyncGetHostByName');
            end;

        trace_socket('< do_lookup_win_socket_server');
      end; // do_lookup_win_socket_server

    procedure c_client_socket.handle_wm_lookup_host(var pv_wm_lookup_host: t_wm_lookup_host);
        // -- triggered when WsaAsyncGet_xxx_ByName is called for Host or Service
      var l_lookup_socket_address_in: tSockAddrIn;
      begin
        trace_socket('');
        trace_socket('> handle_wm_lookup_host');

        // -- can be called for several socket or lookup request
        // --   => has a different lookup handle for each request
        if pv_wm_lookup_host.m_lookup_handle= m_lookup_handle
          then
            begin
              // -- this lookup is no longer needed
              m_lookup_handle:= 0;

              // -- there is a lookup error
              if pv_wm_lookup_host.m_lookup_error<> 0
                then
                  begin
                    // Disconnect(FSocket);
                    // .CreateResFmt(@sWindowsSocketError, [
                    //    SysErrorMessage(pv_wm_lookup_host.m_lookup_error), pv_wm_lookup_host.m_lookup_error,
                    //    'ASync Lookup']);
                    raise_exception(e_lookup_result_error, pv_wm_lookup_host.m_lookup_error, 'wm_lookup');
                  end
                else begin
                    // -- build l_socket_address_in

                    // -- now do the connection
                    // -- fill the IP address
                    FillChar(l_lookup_socket_address_in, sizeof(l_lookup_socket_address_in), 0);

                    with l_lookup_socket_address_in do
                    begin
                      sin_family:= pf_Inet;
                      // -- the requested service
                      // --   not necessary for lookup
                      sin_port:= hToNs(m_remote_port);
                      // -- the server IP address
                      sin_addr.s_addr:= Integer(Pointer(PHostEnt(m_pt_get_host_data).h_addr^)^);

                      // -- for later connection
                      m_remote_ip:= inet_ntoa(sin_addr);
                      trace_socket('lookup_ip '+ inet_ntoa(sin_addr)); // + ', Port '+ IntToStr(m_remote_port));
                    end; // with m_lookup_socket_address_in

                    if Assigned(m_on_after_socket_host_lookup)
                      then m_on_after_socket_host_lookup(Self);
                  end;
          end; // the sockets's lookup handle

        trace_socket('< handle_wm_lookup_host');
      end; // handle_wm_lookup_host

    procedure c_client_socket.do_connect_win_socket(p_server_ip: String; p_port: Integer);
        // -- knows the server's IP address: no lookup needed
      var l_socket_address_in: tSockAddrIn;
          l_ip_z: array[0..255] of char;
          l_result, l_error: Integer;
      begin
        trace_socket('');
        trace_socket('> do_connect '+ f_name_and_handle(m_base_socket_handle));
        FillChar(l_socket_address_in, sizeof(l_socket_address_in), 0);

        with l_socket_address_in do
        begin
          sin_family:= pf_Inet;
          // -- the requested service
          sin_port:= hToNs(p_port);
          // -- the server IP address
          StrPCopy(l_ip_z, p_server_ip);
          sin_addr.s_Addr:= inet_addr(l_ip_z);
          trace_socket('do_connecte to IP '+ inet_ntoa(sin_addr)+ ', Port '+ IntToStr(p_port));
        end; // with m_socket_address_in

        m_remote_port:= p_port;
        m_remote_ip:= p_server_ip;

        // -- connect
        trace_socket(':Connect('+ f_socket_handle_string(m_base_socket_handle)+ ', '
            + IntToStr(m_remote_port)+ ', )');
        l_result:= Connect(m_base_socket_handle, l_socket_address_in,
            sizeof(l_socket_address_in));
        if l_result<> 0
          then
            begin
              l_error:= WSAGetLastError;
              if l_error= wsaEWouldBlock
                then trace_socket('err wsaWouldBlock: wait...')
                else raise_exception(e_connect_error, l_error, 'connect');
            end;

        trace_socket('< do_connect');
      end; // do_connect_win_socket

    // -- Windows notifies our socket

    procedure c_client_socket.handle_wm_async_select(var pv_wm_async_select: t_wm_async_select);
      // -- wParam: hSocket, lo(lParam): notification, hi(lParam): error
      var l_trace_socket: Boolean;

      procedure handle_fd_connect_notification(p_socket: Integer);
        begin
          trace_socket('> fd_connect('+ f_name_and_handle(p_socket)+ ')');

          // -- fill mail boxes
          m_local_port:= f_local_socket_port(m_base_socket_handle);
          m_local_ip:= f_local_socket_ip(m_base_socket_handle);

          // -- just some feed back
          trace_socket(f_socket_name(p_socket));

          m_is_open:= p_socket<> INVALID_SOCKET;

          if Assigned(m_on_after_socket_connected)
            then m_on_after_socket_connected(Self);

          trace_socket('< fd_connect ');
        end; // handle_fd_connect_notification

      procedure handle_fd_write_notification(p_socket: Integer);
        begin
          trace_socket('> fd_write('+ f_name_and_handle(p_socket)+ ')');

          if p_socket= m_base_socket_handle
            then begin
                send_buffered_data;
                If Assigned(m_on_after_socket_wrote_data)
                  then m_on_after_socket_wrote_data(Self);
              end
            else trace_socket('*** fd_write: not_client_socket_handle (ok)'
                    + ' p_sock ='+ IntToStr(p_socket)+ ' cli= '+ IntToStr(m_base_socket_handle));

          trace_socket('< fd_write');
        end; // handle_fd_write_notification

      procedure handle_fd_read_notification(p_socket: tSocket);
        begin
          trace_socket('> fd_read('+ f_name_and_handle(p_socket)+ ')');

          if p_socket= m_base_socket_handle
            then begin
                receive_buffered_data;
                if Assigned(m_on_after_socket_received_data)
                  then m_on_after_socket_received_data(Self);
              end
            else begin
                // -- this should be the same as the creation handle
                // -- but not after accept (-1: Invalid socket_handle)
                trace_socket('*** fd_read: not_client_socket_handle'
                    + ' p_sock ='+ IntToStr(p_socket)+ ' cli= '+ IntToStr(m_base_socket_handle));
              end;

          if l_trace_socket
            then display('< fd_read');
        end; // handle_fd_read_notification

      procedure handle_fd_close_notification(p_socket: Integer);
        begin
          trace_socket('> fd_close('+ f_name_and_handle(p_socket)+ '). The remote server client closed');

          // -- close our client socket
          if p_socket= m_base_socket_handle
            then begin
                if assigned(m_on_after_remote_server_client_socket_closed)
                  then m_on_after_remote_server_client_socket_closed(Self);

                // -- close the socket (this cannot hurt: nothing
                // --   else will be received, and the c_client_socket is still in memory)
                display('server_close -> client_close '+ f_name_and_handle(p_socket));
                do_close_win_socket;
                if m_do_free_after_server_closed
                  then Free;
              end
            else trace_socket('*** fd_close: not_client_socket_handle '
                        + ' p_sock ='+ IntToStr(p_socket)+ ' cli= '+ IntToStr(m_base_socket_handle));

          if l_trace_socket
            then display('< fd_close')
        end; // handle_fd_close_notification

      var l_socket_handle: Integer;
          l_error, l_notification: Integer;

      begin // handle_wm_async_select
        l_trace_socket:= m_trace_socket;

        l_socket_handle:= pv_wm_async_select.m_socket_handle;
        l_error:= pv_wm_async_select.m_select_error;
        l_notification:= pv_wm_async_select.m_select_event;

        trace_socket('');
        trace_socket('> handle_wm_async_select '+ m_name+ ' wnd_hnd=$'+ IntToHex(m_window_handle, 2)
            + ' err=$'+ IntToHex(l_error, 2)
            + ' notif='+ IntToHex(l_notification, 2)
            + ' sck_hnd='+ IntToStr(l_socket_handle));

        if l_error= 0
          then trace_socket('notif: '+ f_socket_notification_name(l_notification))
          else trace_socket('notif: '+ f_socket_notification_name(l_notification)
              + ' err '+ IntToStr(l_error));

        if l_error<= wsaBaseErr
          then begin
              case l_notification of
                FD_CONNECT: handle_fd_connect_notification(l_socket_handle);
                FD_ACCEPT: display_bug_halt('client_cannot_receive_fd_accept');
                FD_WRITE: // -- sent when can write in the buffer (after connect or when buffer is empty)
                          handle_fd_write_notification(l_socket_handle);
                FD_READ: // -- sent when received some data (reception buffer not empty)
                         handle_fd_read_notification(l_socket_handle);

                FD_CLOSE: handle_fd_close_notification(l_socket_handle);
                else trace_socket('unhandled_notification ');
              end // case
            end
          else begin
              if l_notification= FD_CLOSE
                then handle_fd_close_notification(l_socket_handle)
                else raise_exception(e_notification_error, l_error, 'wm_asynch');
            end;

        if l_trace_socket
          then display('< handle_wm_async_select');
      end; // handle_wm_async_select

    procedure c_client_socket.do_close_win_socket;
      var l_result: Integer;
          l_trace_socket: Boolean;
      begin
        trace_socket('');
        trace_socket('> do_close_win_socket '+ f_name_and_handle(m_base_socket_handle));

        l_trace_socket:= m_trace_socket;
        if m_lookup_handle<> 0
          then begin
              l_result:= WSACancelASyncRequest(m_lookup_handle);
              if l_result<> 0
                then raise_exception(e_wsa_cancel_lookup_error, l_result, 'WSACancelASyncRequest');
              m_lookup_handle:= 0;
            end;

        Inherited;

        if l_trace_socket
          then display('< do_close_win_socket');
      end; // do_close_win_socket

    Destructor c_client_socket.Destroy;
      begin
        trace_socket('> c_client_socket.Destroy');

        if m_is_open and (m_base_socket_handle<> INVALID_SOCKET)
          then do_close_win_socket;

        if m_window_handle<> 0
          then DeallocateHWnd(m_window_handle);

        FreeMem(m_pt_get_host_data);
        m_pt_get_host_data:= Nil;

        // -- call WSACleanup

        Inherited;

        trace_socket('< c_client_socket.Destroy');
      end; // Destroy

    begin // u_c_client_socket
    end. // u_c_client_socket


