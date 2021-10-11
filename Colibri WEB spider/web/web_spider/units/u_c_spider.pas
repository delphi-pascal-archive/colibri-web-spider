// 003 u_c_spider
// 13 jun 2005

// -- (C) Felix John COLIBRI 2004
// -- documentation: http://www.felix-colibri.com

// -- used for one domain (the logs and save directory depends on this)
// --  Recreate if domain changes

(*$r+*)

unit u_c_spider;
  interface
    uses Classes, u_c_basic_object
        , u_c_url
        , u_c_log, u_c_log_list
        , u_c_remote_file_list_3
        , u_c_client_socket_4
        , u_c_http_client_socket
        ;

    const k_log_http_header= 'http_header';
          k_log_socket= 'socket';

          k_log_traffic= 'traffic';
          // -- get  // , received, free
          k_log_get= 'get';
          k_log_received_page= 'received_page';
          // -- new pages added to the remote list (other and todo)
          k_log_new_remote= 'new_remote';
          k_log_tag_analysis= 'tag_analysis';
          // -- tag analysis
          k_log_all_tags= 'tag_all';
          k_log_tags_nok= 'tag_nok';
          // -- the new files added
          k_log_new_key= 'new_keys';
          // -- the files removed (? .._ ...)
          k_log_disabled_file= 'disabled_files';

          k_log_protocol= 'protocol';
          k_log_http_protocol_file= 'http_protocol';
          // k_log_html_file= 'files_html';
          // k_log_binary_file= 'files_binary';
          k_log_all_file= 'files_all';
          k_log_get_detail= 'files_get_detail';
          k_log_running= 'running';
          k_log_remote_state= 'remote_state';
          k_log_save_on_server_closed= 'finished_on_server_closed';

    type c_spider= Class; // forward
         t_po_spider_event= procedure(p_c_spider: c_spider;
             p_c_remote_file: c_remote_file) of object;

         c_spider= class(c_basic_object)
                     // -- the first requested url => sets the root domain+path
                     m_c_requested_url: c_url;
                     m_requested_ip: String;
                     m_root_save_path: String;
                     m_c_remove_extension_list, m_c_keep_extension_list: tStringList;

                     m_trace_tag: Boolean;
                     // -- check whether recursive save
                     m_save_level: Integer;

                     m_c_log_list: c_log_list;

                     m_c_remote_file_list: c_remote_file_list;

                     m_on_before_download: t_po_spider_event;
                     m_on_received_data: t_po_spider_event;
                     m_on_after_save: t_po_spider_event;

                     Constructor create_spider(p_name, p_root_save_path: String; p_c_url: c_url);

                     function f_is_parent_page(p_c_url: c_url): Boolean;
                     function f_is_page(p_c_url: c_url): Boolean;
                     function f_is_child_page(p_c_url: c_url): Boolean;
                     function f_is_domain_page(p_c_url: c_url): Boolean;

                     procedure download_the_file(p_c_remote_file: c_remote_file);
                     procedure _handle_after_received_data(p_c_http_client_socket: c_http_client_socket);
                     procedure _handle_after_received_body(p_c_http_client_socket: c_http_client_socket);
                     procedure _handle_after_server_client_socket_closed(p_c_client_socket: c_client_socket);

                     procedure save_the_page(p_c_http_client_socket: c_http_client_socket);
                       function f_is_remove_extension(p_remote_extension: String): Boolean;
                       function f_is_keep_extension(p_remote_extension: String): Boolean;
                     procedure filter_other_pages; Virtual;
                     function f_c_select_next_page: c_remote_file; Virtual;

                     procedure replace_image_tags;

                     Destructor Destroy; Override;
                   end; // c_spider

  implementation
    uses SysUtils, u_c_display
        , u_characters, u_strings
        , u_dir, u_file
        , u_c_remote_html_file_3
        ;

    // -- c_spider

    Constructor c_spider.create_spider(p_name, p_root_save_path: String; p_c_url: c_url);

      procedure create_page_logs;
        var l_log_path: String;
        begin
          if m_c_requested_url.m_domain= ''
            then display_bug_halt('no_g_domain');

          l_log_path:= m_root_save_path+ m_c_requested_url.m_domain+ '\_log\';
          display('log '+ l_log_path);
          f_create_path(l_log_path);

          // -- create an empty "__doc"
          try
            save_string('', m_root_save_path+ m_c_requested_url.m_domain+ '\__doc.txt');
          except
            stop('create__doc');
          end;

          // -- redirect the general log
          // initialize_display_log(l_log_path+ 'log_spider_2.txt');

          m_c_log_list.Free;
          m_c_log_list:= c_log_list.create_log_list(l_log_path);
          with m_c_log_list do
          begin
            add_unique_log(k_log_socket);
            add_unique_log(k_log_http_header);
            add_unique_log(k_log_traffic);

            add_unique_log(k_log_tag_analysis);

            add_unique_log(k_log_get);
            add_unique_log(k_log_protocol);
            add_unique_log(k_log_received_page);
            add_unique_log(k_log_all_tags);
            add_unique_log(k_log_tags_nok);
            add_unique_log(k_log_new_remote);
            add_unique_log(k_log_running);
            add_unique_log(k_log_new_key);
            add_unique_log(k_log_disabled_file);

            add_unique_log(k_log_http_protocol_file);
(*
            add_unique_log(k_log_html_file);
            add_unique_log(k_log_binary_file);
*)
            add_unique_log(k_log_all_file);
            add_unique_log(k_log_get_detail);

            add_unique_log(k_log_remote_state);
            add_unique_log(k_log_save_on_server_closed);
          end;
          // display_bug_halt(l_log_path);
        end; // create_page_log

      begin // create_spider
        Inherited create_basic_object(p_name);

        m_root_save_path:= p_root_save_path;
        m_c_requested_url:= p_c_url;

        create_page_logs;
        m_c_remove_extension_list:= tStringList.Create;
        m_c_keep_extension_list:= tStringList.Create;

        display('create_remote_file');
        m_c_remote_file_list:= c_remote_file_list.create_remote_file_list('remote_file_list',
            m_c_log_list);

        // -- add the first page to the requested page list
        with m_c_requested_url do
        begin
          if m_domain+ m_segment+ m_page+ m_extension<> ''
            then begin
                // -- create and avoid redisplay on the Memo
                with m_c_remote_file_list.f_add_new_remote_file(m_c_requested_url.f_c_clone, m_c_requested_url.f_c_clone) do
                  m_already_displayed:= True;
              end;
        end; // with m_c_requested_url
        display('end_create_spider');
      end; // create_spider

    // -- page checks - level

    function c_spider.f_is_parent_page(p_c_url: c_url): Boolean;
      begin
        with p_c_url do
          Result:= (m_domain= m_c_requested_url.m_domain)
            and (Length(m_segment)< Length(m_c_requested_url.m_segment));
      end; // f_is_parent_page

    function c_spider.f_is_page(p_c_url: c_url): Boolean;
      begin
        with p_c_url do
          Result:= (m_domain= m_c_requested_url.m_domain)
            and (m_segment= m_c_requested_url.m_segment);
      end; // f_is_page

    function c_spider.f_is_child_page(p_c_url: c_url): Boolean;
      begin
        with p_c_url do
          Result:= (m_domain= m_c_requested_url.m_domain)
            and (Pos(m_c_requested_url.m_segment, m_name)> 0)
            and (Length(m_segment)> Length(m_c_requested_url.m_segment));
      end; // f_is_child_page

    function c_spider.f_is_domain_page(p_c_url: c_url): Boolean;
      begin
        Result:= Pos(m_c_requested_url.m_domain, p_c_url.m_name)> 0
      end; // f_is_domain_page

    procedure c_spider.download_the_file(p_c_remote_file: c_remote_file);
        // -- called to get an html page
      var l_get_request: String;
      begin
        display('> download_the_file');

        // -- user display
        if assigned(m_on_before_download)
          then m_on_before_download(Self, p_c_remote_file);

        with p_c_remote_file do
        begin
          display('add_to_running '+ m_name);

          m_c_log_list.write_line(k_log_all_file, m_name);
          // m_c_log_list.write_line(k_log_get_detail, m_remote_domain+ '|'+ m_remote_segment+ '|'+ m_remote_pure_file_name+ '|'+ m_remote_extension);
          // m_c_log_list.write_line(k_log_running, 'add_b '+ m_name);
          // m_c_log_list.write_line(k_log_http_protocol_file, 'GET '+ m_remote_domain+ ' | '+ m_remote_segment+ m_remote_pure_file_name+ m_remote_extension);

          // -- avoid trying if did not complete
          Inc(m_trial_count);
          m_did_get:= True;

          // -- create the c_http_client_socket
          m_c_http_client_socket:= c_http_client_socket.create_http_client_socket('cli');

          with m_c_http_client_socket do
          begin
            m_trace_socket:= True;
            m_c_traffic_log_ref:= m_c_log_list.f_find_by_c_log(k_log_traffic);
            m_do_free_after_server_closed:= True;

            m_c_object:= p_c_remote_file;

            // -- some bytes arrived
            m_on_http_received_data:= _handle_after_received_data;
            // -- received all the Content-Length bytes
            m_on_after_received_http_body:= _handle_after_received_body;
            // -- the server closed its socket
            m_on_after_remote_server_client_socket_closed:= _handle_after_server_client_socket_closed;

            if (m_requested_ip<> '')
                and (m_c_requested_url.m_domain= p_c_remote_file.m_c_normalized_url.m_domain)
              then m_remote_ip:= m_requested_ip;

            // -- sent the GET
            with m_c_normalized_url do
            begin
              l_get_request:= 'GET '+ m_segment+ m_page+ m_extension+ ' HTTP/1.0'+ k_new_line
                 + 'Accept: image/gif, image/x-xbitmap, image/jpeg, image/pjpeg, */*'+ k_new_line
                 // + 'Accept-Encoding: gzip, deflate'+ k_new_line
                 + 'User-Agent: Mozilla/4.0 (compatible; MSIE 6.0; Windows NT 5.1)'+ k_new_line
                 + 'Host: '+ m_domain+ k_new_line
                 // + 'Connection: Keep-Alive'+ k_new_line
                 + k_new_line;
              m_c_log_list.write_line(k_log_get, m_segment+ m_page+ m_extension);

              // -- for the first page, must do a lookup
              if m_remote_ip= ''
                then lookup_connect_get(m_domain, k_http_port, l_get_request)
                else connect_get(m_remote_ip, k_http_port, l_get_request);
            end; // with m_c_normalized_url
          end; // with g_c_http_client
        end; // with p_c_remote_file
        display('< download_the_file');
      end; // download_the_file

    procedure c_spider._handle_after_received_data(p_c_http_client_socket: c_http_client_socket);
        // -- received a couple of bytes. Notify the main form
      begin
        if Assigned(m_on_received_data)
          then m_on_received_data(Self, c_remote_file(p_c_http_client_socket.m_c_object));
      end; // _handle_after_received_data

    procedure c_spider._handle_after_received_body(p_c_http_client_socket: c_http_client_socket);
        // -- received ALL the body bytes
      begin
        with c_remote_file(p_c_http_client_socket.m_c_object) do
        begin
          display('saved_?_received '+ f_display_tf(m_saved)+ ' '+ m_name);
          if not m_saved
            then begin
                m_saved:= true;
                m_c_log_list.write_line(k_log_save_on_server_closed, m_name);
                display('> _handle_after_received_body ');
                save_the_page(p_c_http_client_socket);
                display('< _handle_after_received_body');
              end;
        end;
      end; // handle_after_received_html_content

    procedure c_spider._handle_after_server_client_socket_closed(p_c_client_socket: c_client_socket);
        // -- the server closed its socket. If not already saved, save
      begin
        with c_remote_file(p_c_client_socket.m_c_object) do
        begin
          display('saved_?_closed '+ f_display_tf(m_saved)+ ' '+ m_name);
          if not m_saved
            then begin
                m_saved:= true;
                display('> _handle_after_server_client_socket_closed');
                save_the_page(p_c_client_socket as c_http_client_socket);
                display('< _handle_after_server_client_socket_closed');
              end;
        end;
        // -- free the c_http_client_socket
      end; // _handle_after_server_client_socket_closed

    procedure c_spider.save_the_page(p_c_http_client_socket: c_http_client_socket);

      procedure save_the_html_file(p_c_remote_html_file: c_remote_html_file;
          p_save_full_file_name: String);
          // -- save the page
        begin
          display('> save_the_html_file');

          with p_c_http_client_socket, p_c_remote_html_file do
          begin
            // m_c_log_list.write_line(k_log_html_file, '  '+ l_url);

            // -- ?? can still have C:\data_download\browser\trial_1\c2.com\\cgi/wiki_AntiPatternsCatalog
            // -- if came from "server_closed"
            save_file(p_save_full_file_name);

            // -- copy the text for analysis (use a c_text_buffer to be able
            // --   to analyze files from disc)
            with m_c_reception_buffer do
              copy_data_to_c_text_buffer(@ m_oa_byte_buffer[m_end_of_header_index+ 4],
                  m_downloaded_content_length);

            // -- this will build a tag list (all absolute, no .., if no domain did add one)
            m_trace_tag_analysis:= m_trace_tag;
            // -- switch to this display
            // m_c_tag_analysis_strings:= Form1.tags_memo_.Lines;
(*
            if m_trace_tag_analysis
              then initialize_display(Form1.tags_memo_.Lines);
*)
            analyze_text_buffer(m_c_text_buffer,
                m_c_log_list.f_find_by_c_log(k_log_all_tags),
                m_c_log_list.f_find_by_c_log(k_log_tag_analysis));

            m_c_tag_list.display_tag_list;

            // -- add the new tags to the remote_file_list
            m_c_remote_file_list.add_new_tags(m_c_tag_list,  m_name);
(*
            // -- switch to this display
            if m_trace_tag_analysis
              then initialize_display(Form1.Memo1.Lines);
*)
          end; // with p_c_http_client_socket, p_c_remote_html_file

          display('< save_the_html_file');
        end; // save_the_html_file

      function f_has_html_content: Boolean;
          // -- check if contains <HTML> and </HTML>
        begin
          with p_c_http_client_socket.m_c_reception_buffer do
          begin
            if ((f_pos('<HTML')>= 0) or (f_pos('<html')>= 0))
              and
                ((f_pos('</HTML>')>= 0) or (f_pos('</html>')>= 0))
              then begin
                  display('HTML !!!');
                  Result:= True;
                end
              else Result:= False;
          end;
        end; // f_has_html_content

      function f_c_change_to_html_file(p_c_remote_file: c_remote_file; p_index_of: Integer): c_remote_html_file;
          // -- promote the c_remote_file to a c_remote_html_file
        begin
          with p_c_remote_file do
          begin
            Result:= c_remote_html_file.create_remote_html_file(
                m_c_url, m_c_normalized_url);

            Result.m_c_http_client_socket:= m_c_http_client_socket;
            // -- the other booleans
            Result.m_already_displayed:= m_already_displayed;
            Result.m_did_get:= m_did_get;
            Result.m_saved:= m_saved;
            Result.m_c_parent_remote_file_list:= m_c_parent_remote_file_list; 

            // -- nil those since used in the other
            m_c_url:= Nil; m_c_normalized_url:= Nil; m_c_dos_save_names:= Nil;
            m_c_http_client_socket:= Nil;
            Free;

            m_c_remote_file_list.m_c_remote_file_list.Objects[p_index_of]:= c_remote_file(Result);
            // -- also swap the object in the socket class
            p_c_http_client_socket.m_c_object:= c_remote_file(Result);
          end; // with p_c_remote_file
        end; // f_c_change_to_html_file

      var l_c_remote_file: c_remote_file;
          l_save_path, l_save_file_name, l_save_extension: String;
          l_is_html: Boolean;
          l_index_of: Integer;
          l_save_full_file_name: String;

          l_c_next_remote_file: c_remote_file;

      begin // save_the_page
        display('> save_the_page');
        Inc(m_save_level);

        with p_c_http_client_socket do
        begin
          l_c_remote_file:= c_remote_file(m_c_object);
          display(l_c_remote_file.m_name+ ' '+ f_display_tf(l_c_remote_file.m_saved)+ ' '+ IntToStr(m_save_level));

          if (m_requested_ip= '')
            and (m_c_requested_url.m_domain= l_c_remote_file.m_c_normalized_url.m_domain)
              then m_requested_ip:= m_remote_ip;

          m_c_log_list.write_line(k_log_received_page, l_c_remote_file.m_name);
          m_c_log_list.write_line(k_log_get, '  '+ l_c_remote_file.m_name);

          l_c_remote_file.m_c_dos_save_names:= l_c_remote_file.m_c_normalized_url.f_c_dos_save_names;

          l_is_html:= True;

          with l_c_remote_file, m_c_dos_save_names do
          begin
            display('dos '+ f_display_url);
            // -- save the page
            if m_downloaded_content_length<> 0
              then begin
                  l_save_file_name:= m_page;

                  if m_answer_code= 404
                    then l_save_file_name:= '_404_'+ l_save_file_name;

                  l_save_extension:= m_extension;
                  if l_c_remote_file is c_remote_html_file
                    then begin
                        if l_save_extension= ''
                          then l_save_extension:= '_.html';
                      end
                    else begin
                        // -- check whether bytes contains <HTML> tags
                        if f_has_html_content
                          then begin
                              // -- replace the pointer in the LIST
                              l_index_of:= m_c_remote_file_list.f_index_of(m_name);
                              if l_index_of>= 0
                                then begin
                                    if m_extension= ''
                                      then l_save_extension:= '___.html'
                                      else l_save_extension:= '__.html';
                                    l_c_remote_file:= f_c_change_to_html_file(l_c_remote_file, l_index_of);
                                  end
                                else begin
                                    display_bug_stop('*** could_not_relocate binary_html');
                                    l_is_html:= False;
                                    if l_save_extension= ''
                                      then l_save_extension:= '_.txt';
                                  end;
                            end
                          else begin
                              l_is_html:= False;
                              if l_save_extension= ''
                                then l_save_extension:= '_.txt';
                            end;
                      end;

                  // -- here has a name, an extension, and html or not
                  l_save_path:= m_root_save_path+ m_domain+ m_segment;
                  f_create_path(l_save_path);
                  l_save_full_file_name:= f_unique_file_name(l_save_path+ l_save_file_name+ l_save_extension);
                  display('save %'+ l_save_path+ '%'+ l_save_file_name+ '%'+ l_save_extension+ '%');
                  m_saved_name:= l_save_full_file_name;

                  if l_is_html
                    then save_the_html_file(c_remote_html_file(l_c_remote_file), l_save_full_file_name)
                    else l_c_remote_file.save_file(l_save_full_file_name);
                end // f_content_size
              else begin
                  // -- if no content, and no exention, try with / extension
                  if m_c_url.m_extension= ''
                    then begin
                        display('+++ no_ext: try with /');
                      end;
                end;

            // -- do not download again
            m_downloaded:= True;
          end; // with c_remote_file(m_c_object)
        end; // p_c_http_client_socket

        // -- remove the "non downloadable files" from the list
        filter_other_pages;

        l_c_next_remote_file:= f_c_select_next_page;
        // l_c_next_remote_file:= Nil;

        // -- notify the form
        // -- update display TODO / OTHER
        if Assigned(m_on_after_save)
          then m_on_after_save(Self, l_c_remote_file);

        // -- call the download
        if l_c_next_remote_file<> Nil
          then download_the_file(l_c_next_remote_file);

        Dec(m_save_level);
        display('< save_the_page');
      end; // save_the_page

    function c_spider.f_is_remove_extension(p_remote_extension: String): Boolean;
        //-- do not download ".avi" etc
      var l_remove_index: Integer;
      begin
        Result:= False;
        with m_c_remove_extension_list do
          for l_remove_index:= 0 to Count- 1 do
            if LowerCase(Strings[l_remove_index])= p_remote_extension
              then begin
                  Result:= True;
                  Break;
                end;
      end; // f_remove_extension

    function c_spider.f_is_keep_extension(p_remote_extension: String): Boolean;
      var l_keep_index: Integer;
      begin
        Result:= Pos('.*', m_c_keep_extension_list.Text)> 0;

        if not Result
          then
            with m_c_keep_extension_list do
              for l_keep_index:= 0 to Count- 1 do
                if LowerCase(Strings[l_keep_index])= p_remote_extension
                  then begin
                      Result:= True;
                      Break;
                    end;
      end; // f_is_keep_extension

    procedure c_spider.filter_other_pages;
        // -- remove unwanted pages. Change with user's desire
      var l_remote_file_index: Integer;
      begin
        display('> filter_other_pages');

        with m_c_remote_file_list do
        begin
          for l_remote_file_index:= 0 to f_remote_file_count- 1 do
            with f_c_remote_file(l_remote_file_index) do
              if not m_downloaded and m_do_download
                then begin
                    // -- remove entries with
                    // --  - still a CGI parameter ?
                    // --  - no file name (a pure path) no: send (the server might have a segment default page
                    // --  - a non-resolved local path

                    if (LowerCase(m_c_normalized_url.m_domain)<> LowerCase(m_c_requested_url.m_domain))
                        // or (Pos('?', m_remote_segment+ m_remote_pure_file_name+ m_remote_extension)> 0)
                        // or (m_remote_extension= '.gif')
                        // or (Pos('../', m_c_url.m_segment)> 0)
                        // or (Pos('papers', LowerCase(m_remote_segment))> 0)
                        or (Pos('javadoc', LowerCase(m_c_normalized_url.m_segment))> 0)
                        // or (m_remote_pure_file_name= '')
                        or (Pos('cv', m_c_normalized_url.m_page) > 0)

                        or (m_c_normalized_url.m_page= '')
                        or f_is_remove_extension(LowerCase(m_c_url.m_extension))
                        or (not f_is_keep_extension(LowerCase(m_c_url.m_extension)))
                      then begin
                          m_do_download:= False;
                          // -- add to the "other" list
                          display('remove '+ m_name);
                          m_c_log_list.write_line(k_log_disabled_file, m_name);
                        end
                  end;
        end; // with m_c_remote_file_list

        display('< filter_other_pages');
      end; // filter_other_pages

    function c_spider.f_c_select_next_page: c_remote_file;
        // -- selects the next page. Change with user's strategy
      var l_remote_file_index: Integer;
          l_trial: Integer;
          l_requested_segment: String;
      begin
        display('> f_c_select_nect_file ');

        l_requested_segment:= m_c_requested_url.m_segment;
        // -- max trial count
        l_trial:= 0;

        with m_c_remote_file_list do
        begin
          // display(f_remaining_file_stats(m_c_requested_url.m_domain));

          // -- first get all the .ZIP and .IMG from the current page
          // --   not if already tried and failed
          // --   and from same or included segment
          for l_remote_file_index:= 0 to f_remote_file_count- 1 do
            with f_c_remote_file(l_remote_file_index) do
              if not m_downloaded and m_do_download and (m_trial_count<= l_trial) 
                  and not m_c_url.f_is_html_extension
                  and
                      (
                          (Pos(l_requested_segment, m_name)> 0)
                       or
                          (m_c_normalized_url.m_domain= m_c_requested_url.m_domain)
                       )
                then begin
                    Result:= f_c_self;
                    display('< f_c_select_nect_file '+ Result.m_name);
                    Exit;
                  end;

           // -- no more downloadable pages
           Result:= Nil;

           display('> not');
           display_detailed_remote_file_list;
           display('< not');
        end; // with m_c_remote_file_list

        display('< f_c_select_next_page ');
      end; // f_c_select_next_page

    procedure c_spider.replace_image_tags;
      begin
        m_c_remote_file_list.replace_image_tags;
      end; // replace_image_tags

    Destructor c_spider.Destroy;
      begin
        m_c_requested_url.free;
        m_c_remove_extension_list.Free;
        m_c_keep_extension_list.Free;
        m_c_log_list.Free;
        m_c_remote_file_list.Free;

        Inherited;
      end; // Destroy

    begin // u_c_spider
    end. // u_c_spider


