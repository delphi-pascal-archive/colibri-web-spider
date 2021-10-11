// 003 u_c_remote_file_list_3
// 12 jun 2005

// -- (C) Felix John COLIBRI 2004
// -- documentation: http://www.felix-colibri.com

// -- a list of domain | segment | file+extension

// -- leaves:
// --  - can be .html, .gif, .zip, .pdf ...
// --  - has a key which is the unique "domain+ segment+ page+ extension"
// --  - root path on C:, containing domain/path/full_file_name
// --  - booleans: downloaded or not

(*$r+*)

unit u_c_remote_file_list_3;
  interface
    uses Classes, u_c_basic_object
        , u_c_log_list
        , u_c_url
        , u_c_tag_list
        , u_c_http_client_socket
        ;

    type c_remote_file_list= Class; // forward

         c_remote_file= // one "remote_file"
                        Class(c_basic_object)
                          // -- m_name: unique name for donwnload
                          // --  => concatenation of: http_domain+ UNIQUE http_requested_page_name

                          m_c_parent_remote_file_list: c_remote_file_list;

                          m_c_url: c_url;
                          // -- remove %nn, HOME_absolute
                          m_c_normalized_url: c_url;
                          // -- no \, remove spaces, ? *, cgi parameters normalized or removed
                          m_c_dos_save_names: c_url;

                          m_c_http_client_socket: c_http_client_socket;

                          // -- all saved, analyzed
                          m_downloaded: Boolean;
                          // -- only try to get once
                          m_trial_count: Integer;
                          // -- do not try to download those (other domains, avi ...)
                          m_do_download: Boolean;
                          // -- update display on next event
                          m_already_displayed: Boolean;
                          // -- to check consistency with saved
                          m_did_get: Boolean;
                          // -- if analyses when content received, avoid doing it when server_closes
                          m_saved: Boolean;

                          // -- the name in the local dir, used to replace the image tags
                          m_saved_name: String;

                          Constructor create_remote_file(p_c_url, p_c_normalized_url: c_url);

                          function f_display_remote_file: String;
                          function f_c_self: c_remote_file;

                          procedure save_file(p_full_file_name: String);
                          procedure save_to_list(p_c_list: tStrings);

                          Destructor Destroy; Override;
                        end; // c_remote_file

         c_remote_file_list= // "remote_file" list
                             Class(c_basic_object)
                               // -- m_name: the ulr "www.patern.html/joel/tutorial.htm"
                               m_c_remote_file_list: tStringList;
                               m_c_log_list_ref: c_log_list;

                               // m_downloaded_count, m_to_download_count, m_other_domain_count, m_trial_count: Integer;

                               Constructor create_remote_file_list(p_name: String;
                                    p_c_log_list_ref: c_log_list);

                               function f_remote_file_count: Integer;
                               function f_c_remote_file(p_remote_file_index: Integer): c_remote_file;
                               function f_index_of(p_remote_file_name: String): Integer;
                               function f_c_find_by_remote_file(p_remote_file_name: String): c_remote_file;
                               procedure add_remote_file(p_remote_file_name: String; p_c_remote_file: c_remote_file);
                               function f_add_new_remote_file(p_c_url, p_c_normalized_url: c_url):  c_remote_file;

                               procedure display_remote_file_list;
                               procedure display_detailed_remote_file_list;
                               procedure add_new_tags(p_c_tag_list: c_tag_list;
                                   p_page_name: String); 

                               procedure save_to_file(p_full_file_name: String);
                               procedure add_remote_file_list_to_log;
                               function f_remaining_file_stats(p_domain: String): String;

                               procedure replace_image_tags;

                               Destructor Destroy; Override;
                             end; // c_remote_file_list

  implementation
    uses SysUtils, TypInfo, u_c_display, u_c_log
        , u_strings, u_stringlist
        , u_dir, u_file
        , u_c_remote_html_file_3
        , u_c_spider
        ;

    // -- c_remote_file

    Constructor c_remote_file.create_remote_file(p_c_url, p_c_normalized_url: c_url);
      begin
        // -- the KEY is based on the normalized url (no %nn, no "..\")
        Inherited create_basic_object(p_c_normalized_url.m_name);

        m_c_url:= p_c_url;
        m_c_normalized_url:= p_c_normalized_url;

        m_do_download:= True;
      end; // create_child_line

    function c_remote_file.f_display_remote_file: String;
      begin
        Result:= f_display_TF(m_downloaded)+ Format('  %-10s ', [m_name]);
      end; // f_display_remote_file

    function c_remote_file.f_c_self: c_remote_file;
      begin
        Result:= Self;
      end; // f_c_self

    procedure c_remote_file.save_file(p_full_file_name: String);
      begin
        with m_c_http_client_socket, m_c_reception_buffer do
        begin
          display('> save_file '+ p_full_file_name);
          save_to_file_start_end(p_full_file_name, m_end_of_header_index+ 4, m_write_index- 1);
          display('< save_file '+ IntToStr(m_write_index- (m_end_of_header_index+ 4)));
        end;
      end; // save_file

    procedure c_remote_file.save_to_list(p_c_list: tStrings);
      begin
        display('save_to_list '+ m_name);
        p_c_list.Add(m_c_normalized_url.f_display_url);
      end; // save_to_list

    Destructor c_remote_file.Destroy;
      begin
        m_c_url.Free;
        m_c_normalized_url.Free;
        m_c_dos_save_names.Free;

        InHerited;
      end; // Destroy

    // -- c_remote_file_list

    Constructor c_remote_file_list.create_remote_file_list(p_name: String;
        p_c_log_list_ref: c_log_list);
      begin
        Inherited create_basic_object(p_name);

        m_c_remote_file_list:= tStringList.Create;
        m_c_log_list_ref:= p_c_log_list_ref;
      end; // create_remote_file_line

    function c_remote_file_list.f_remote_file_count: Integer;
      begin
        Result:= m_c_remote_file_list.Count;
      end; // f_remote_file_count

    function c_remote_file_list.f_c_remote_file(p_remote_file_index: Integer): c_remote_file;
      begin
        Result:= c_remote_file(m_c_remote_file_list.Objects[p_remote_file_index]);
      end; //  f_c_remote_file

    function c_remote_file_list.f_index_of(p_remote_file_name: String): Integer;
      begin
        Result:= m_c_remote_file_list.IndexOf(p_remote_file_name);
      end; // f_index_of

    function c_remote_file_list.f_c_find_by_remote_file(p_remote_file_name: String): c_remote_file;
      var l_index_of: Integer;
      begin
        l_index_of:= f_index_of(p_remote_file_name);
        if l_index_of< 0
          then Result:= Nil
          else Result:= c_remote_file(m_c_remote_file_list.Objects[l_index_of]);
      end; // f_c_find_by_name

    procedure c_remote_file_list.add_remote_file(p_remote_file_name: String; p_c_remote_file: c_remote_file);
      begin
        m_c_log_list_ref.write_line(k_log_new_key, p_remote_file_name);

        m_c_remote_file_list.AddObject(p_remote_file_name, p_c_remote_file);
        p_c_remote_file.m_c_parent_remote_file_list:= Self;
      end; // add_remote_file

    function c_remote_file_list.f_add_new_remote_file(p_c_url, p_c_normalized_url: c_url): c_remote_file;
      var l_normalized_name: String;
      begin
        // -- sanity
        if p_c_url= p_c_normalized_url
          then display_bug_halt('use_clone '+ p_c_normalized_url.m_name);

        Result:= Nil;
        l_normalized_name:= p_c_normalized_url.m_name;

        // -- only add new files
        if f_index_of(l_normalized_name)< 0
          then begin
              if p_c_normalized_url.f_is_html_extension
                then begin
                    display('create_new_html_page');
                    Result:= c_remote_html_file.create_remote_html_file(
                        p_c_url, p_c_normalized_url);
                  end
                else begin
                    display('create_new_binary_page');
                    Result:= c_remote_file.create_remote_file(
                        p_c_url, p_c_normalized_url);
                 end;

              add_remote_file(l_normalized_name, Result);
            end
          else display('already '+ l_normalized_name);
      end; // f_c_add_new_remote_file

    procedure c_remote_file_list.display_remote_file_list;
      var l_remote_file_index: Integer;
      begin
        display(m_name+ ' '+ IntToStr(f_remote_file_count));

        for l_remote_file_index:= 0 to f_remote_file_count- 1 do
          display(f_c_remote_file(l_remote_file_index).f_display_remote_file);
      end; // display_remote_file_list

    procedure c_remote_file_list.display_detailed_remote_file_list;
      var l_remote_file_index: Integer;
          l_display: String;
      begin
        display(m_name+ ' '+ IntToStr(f_remote_file_count));

        for l_remote_file_index:= 0 to f_remote_file_count- 1 do
          with f_c_remote_file(l_remote_file_index) do
          begin
            l_display:= 'enab= '+ f_display_TF(m_do_download)
                + ' down= '+ f_display_TF(m_downloaded)
                + ' name= '+ m_name
                + m_c_url.f_display_url;
            display(l_display);
          end;
      end; // display_detailed_remote_file_list

    procedure c_remote_file_list.add_remote_file_list_to_log;
      var l_remote_file_index: Integer;
          l_html: String;
      begin
        with m_c_log_list_ref.f_find_by_c_log(k_log_remote_state) do
        begin
          write_line('');
          write_line('=========');

          for l_remote_file_index:= 0 to f_remote_file_count- 1 do
            with f_c_remote_file(l_remote_file_index) do
            begin
              if f_c_self is c_remote_html_file
                then l_html:= 'html '
                else l_html:= 'bin  ';

              // save_to_list(l_c_save_list);
              write_line(Format('%3d ', [l_remote_file_index])+ l_html
                  + ' do='+ f_display_TF(m_downloaded)
                  + ' tr='+ Format('%3d ', [m_trial_count])
                  + ' en='+ f_display_TF(m_do_download)
                  + ' get=' + f_display_TF(m_did_get)
                  + ' sv= '+ f_display_TF(m_saved)
                  + ' '+ m_name);
            end; // for l_remote_file_index
        end; // with p_c_log
      end; // add_remote_file_list_to_log

    // -- file

    procedure c_remote_file_list.save_to_file(p_full_file_name: String);
        // -- debug: save the list names (not its files)
      var l_c_save_list: tStringList;
          l_remote_file_index: Integer;
          l_html: String;
      begin
        display('> save_to_file '+ p_full_file_name);
        l_c_save_list:= tStringList.Create;

        for l_remote_file_index:= 0 to f_remote_file_count- 1 do
          with f_c_remote_file(l_remote_file_index) do
          begin
            if f_c_self is c_remote_html_file
              then l_html:= 'html '
              else l_html:= 'bin  ';

            if m_already_displayed
              then l_html:= l_html+ ' al '
              else l_html:= l_html+ '    ';

            l_c_save_list.Add(Format('%3d ', [l_remote_file_index])+ l_html
                + m_c_normalized_url.f_display_url_short);
          end;

        l_c_save_list.Add('');
        l_c_save_list.Add('=====');

        m_c_remote_file_list.Sorted:= True;
        for l_remote_file_index:= 0 to f_remote_file_count- 1 do
          with f_c_remote_file(l_remote_file_index) do
            l_c_save_list.Add(IntToStr(l_remote_file_index)+ ' '+ m_name);

        save_and_free_stringlist(l_c_save_list, p_full_file_name);

        display('< save_to_file');
      end; // save_to_file

    procedure c_remote_file_list.add_new_tags(p_c_tag_list: c_tag_list;
        p_page_name: String);
      // -- has analyzed an html page: now add the new tags to the files to download
      var l_new_tag_count: Integer;
          l_tag_index: Integer;
          l_c_log: c_log;
      begin
        l_c_log:= m_c_log_list_ref.f_find_by_c_log(k_log_new_remote);

        with p_c_tag_list do
        begin
          display('> add_new_files_to_download. Tag_cnt= '+ IntToStr(f_tag_count));
          l_new_tag_count:= 0;

          l_c_log.write_line('');
          l_c_log.write_line('from_page '+ p_page_name);

          for l_tag_index:= 0 to f_tag_count- 1 do
            with f_c_tag(l_tag_index) do
            begin
              display_line;
              display(IntToStr(l_tag_index)+ '_add_? '+ f_display_tag_detail);

              if (m_name<> '') and (Self.f_index_of(m_name)< 0)
                then begin
                    display('add_new_tag >'+ m_name+ '<');
                    f_add_new_remote_file(m_c_url.f_c_clone, m_c_normalized_url.f_c_clone);
                    l_c_log.write_line(m_c_normalized_url.f_display_url);
                  end
                else display('name_empty: not_http_or_local ');
            end; // for l_tag, with f_c_tag

          display('< add_new_tags '+ IntToStr(l_new_tag_count));
        end; // with p_c_tag_list
      end; // add_new_tags

    function c_remote_file_list.f_remaining_file_stats(p_domain: String): String;
        // -- debug: status
      var l_remote_file_index: Integer;
          l_downloaded_count, l_available_count, l_other_domain_count, l_trial_count: Integer;
          l_get_count, l_saved_count: Integer;
      begin
        l_downloaded_count:= 0; l_available_count:= 0; l_other_domain_count:= 0; l_trial_count:= 0;
        l_get_count:= 0; l_saved_count:= 0;

        for l_remote_file_index:= 0 to f_remote_file_count- 1 do
          with f_c_remote_file(l_remote_file_index) do
          begin
            if m_did_get
              then Inc(l_get_count);
            if m_saved
              then Inc(l_saved_count);

            if m_downloaded
              then Inc(l_downloaded_count)
              else begin
                  if m_trial_count> 0
                    then Inc(l_trial_count)
                    else
                      if m_do_download
                        then Inc(l_available_count)
                        else Inc(l_other_domain_count);
                end;
          end;

        Result:= Format('%3d %3d  %3d %3d', [l_downloaded_count, l_available_count, l_other_domain_count, l_trial_count]);
        Result:= Result+ Format(' (%3d %3d)', [l_get_count, l_saved_count]);
      end; // f_remaining_file_stats

    procedure c_remote_file_list.replace_image_tags;
      var l_remote_file_index: Integer;
      begin
        for l_remote_file_index:= 0 to f_remote_file_count- 1 do
          with f_c_remote_file(l_remote_file_index) do
            if m_downloaded and (f_c_self is c_remote_html_file)
              then c_remote_html_file(f_c_self).replace_image_tags_in_html;
      end; // replace_image_tags

    Destructor c_remote_file_list.Destroy;
      var l_remote_file_index: Integer;
      begin
        for l_remote_file_index:= 0 to f_remote_file_count- 1 do
          f_c_remote_file(l_remote_file_index).Free;
        m_c_remote_file_list.Free;

        Inherited;
      end; // Destroy

    begin // u_c_remote_file_list
    end. // u_c_remote_file_list
