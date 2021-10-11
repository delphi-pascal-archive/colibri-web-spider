// 003 u_c_site_spider
// 13 jun 2005

// -- (C) Felix John COLIBRI 2004
// -- documentation: http://www.felix-colibri.com

// -- recursive site download with filtering and selection

(*$r+*)

unit u_c_site_spider;
  interface
    uses Classes, u_c_basic_object
        , u_c_url
        , u_c_log, u_c_log_list
        , u_c_remote_file_list_3
        , u_c_client_socket_4
        , u_c_http_client_socket
        , u_c_spider
        ;

    type t_po_accept_url= procedure(p_c_url: c_url; var pv_accept: Boolean) of Object;

         t_page_level_type= (e_unknown_level, e_outside_level, e_parent_level, e_page_level, e_child_level);
         t_page_type= (e_unknown_type, e_no_type, e_html_type, e_image_type, e_zip_type, e_pdf_type, e_all_type);
         t_page_selection= array[e_unknown_level..e_child_level, e_unknown_type..e_all_type] of Boolean;

         t_fo_check_url= function(p_c_url: c_url): Boolean of Object;
         t_fn_check_url= function(p_c_url: c_url): Boolean;

         c_site_spider= class(c_spider)
                          m_trial_count_max: Integer;
                          m_trace_filter, m_trace_selection: Boolean;

                          m_selection_array: t_page_selection;
                          m_page_level_fo_array: array[e_parent_level..e_child_level] of t_fo_check_url;
                          m_page_type_fn_array: array[e_no_type..e_all_type] of t_fn_check_url;

                          m_on_filter_url: t_po_accept_url;
                          m_oa_on_select_url: array of t_po_accept_url;

                          Constructor create_site_spider(p_name, p_root_save_path: String; p_c_url: c_url);

                          procedure set_selection(p_page_level_type: t_page_level_type;
                              p_page_type: t_page_type; p_set: Boolean);
                          procedure display_selection;
                          function f_has_gui_selection: Boolean;

                          procedure append_selection_procedure(p_po_on_select_url: t_po_accept_url);

                          function f_url_type(p_c_url: c_url): String;

                          procedure filter_other_pages; Override;
                          procedure test_filter_other_pages(p_c_strings: tStrings);

                            procedure select_next_page_default(p_c_url: c_url; var pv_accept: Boolean);
                            procedure select_next_page_default_child(p_c_url: c_url; var pv_accept: Boolean);
                            procedure select_next_page_default_parent_images(p_c_url: c_url; var pv_accept: Boolean);
                          function f_c_select_next_page: c_remote_file; Override;

                          Destructor Destroy; Override;
                        end; // c_site_spider

    function f_page_level_type_name(p_page_level_type: t_page_level_type): String;
    function f_page_type_name(p_page_type: t_page_type): String;

  implementation
    uses SysUtils, TypInfo, u_c_display
        , u_characters, u_strings
        , u_dir, u_file
        , u_c_remote_html_file_3
        ;

    function f_page_level_type_name(p_page_level_type: t_page_level_type): String;
      begin
        Result:= GetEnumName(TypeInfo(t_page_level_type), Integer(p_page_level_type));
        Delete(Result, 1, 2);
        Delete(Result, Length(Result)- Length('_level')+ 1, Length('_level'));
      end; // f_page_level_type_name

    function f_page_type_name(p_page_type: t_page_type): String;
      begin
        Result:= GetEnumName(TypeInfo(t_page_type), Integer(p_page_type));
        Delete(Result, 1, 2);
        Delete(Result, Length(Result)- Length('_type')+ 1, Length('_type'));
      end; // f_page_type_name

    // -- c_site_spider

    Constructor c_site_spider.create_site_spider(p_name, p_root_save_path: String; p_c_url: c_url);
      begin
        Inherited create_spider(p_name, p_root_save_path, p_c_url);
        m_trial_count_max:= 1;

        m_page_level_fo_array[e_parent_level]:= f_is_parent_page;
        m_page_level_fo_array[e_page_level]:= f_is_page;
        m_page_level_fo_array[e_child_level]:= f_is_child_page;

        m_page_type_fn_array[e_no_type]:= f_refuse_all;
        m_page_type_fn_array[e_html_type]:= f_is_html_extension;
        m_page_type_fn_array[e_image_type]:= f_is_image_extension;
        m_page_type_fn_array[e_zip_type]:= f_is_zip_extension;
        m_page_type_fn_array[e_pdf_type]:= f_is_pdf_extension;
        m_page_type_fn_array[e_all_type]:= f_accept_all;
      end; // create_site_spider

    // -- gui selection management

    procedure c_site_spider.append_selection_procedure(p_po_on_select_url: t_po_accept_url);
      begin
        SetLength(m_oa_on_select_url, Length(m_oa_on_select_url)+ 1);
        m_oa_on_select_url[Length(m_oa_on_select_url)- 1]:= p_po_on_select_url;
      end; // append_selection_procedure

    procedure c_site_spider.set_selection(p_page_level_type: t_page_level_type;
        p_page_type: t_page_type; p_set: Boolean);
      begin
        if (p_page_level_type<> e_unknown_level) and (p_page_type<> e_unknown_type)
          then
            m_selection_array[p_page_level_type, p_page_type]:= p_set;
      end; // set_selection

    procedure c_site_spider.display_selection;
      var l_page_level_type: t_page_level_type;
          l_page_type: t_page_type;
          l_level: String;
      begin
        display_line;

        for l_page_level_type:= e_parent_level to e_child_level do
        begin
          l_level:= f_page_level_type_name(l_page_level_type);
          for l_page_type:= e_no_type to e_all_type do
            if m_selection_array[l_page_level_type, l_page_type]
              then begin
                  if l_level<> ''
                    then begin
                        display(l_level);
                        l_level:= '';
                      end;
                  display('  '+ f_page_type_name(l_page_type));
                end;
        end;
      end; // display_selection

    function c_site_spider.f_has_gui_selection: Boolean;
      var l_page_level_type: t_page_level_type;
          l_page_type: t_page_type;
      begin
        Result:= False;
        for l_page_level_type:= e_parent_level to e_child_level do
          for l_page_type:= e_no_type to e_all_type do
            if m_selection_array[l_page_level_type, l_page_type]
              then begin
                  Result:= True;
                  Break;
                end;
      end; // f_has_gui_selection

    // -- remove undesired pages

    function c_site_spider.f_url_type(p_c_url: c_url): String;
      begin
        if f_is_page(p_c_url)
          then Result:= 'page ' else
        if f_is_child_page(p_c_url)
          then Result:= 'child ' else
        if f_is_parent_page(p_c_url)
          then Result:= 'parent '
        else Result:= '??? ';

        if f_is_html_extension(p_c_url)
          then Result:= Result+ ' html ' else
        if f_is_image_extension(p_c_url)
          then Result:= Result+ ' image ' else
        if f_is_zip_extension(p_c_url)
          then Result:= Result+ ' zip ' else
        if f_is_pdf_extension(p_c_url)
          then Result:= Result+ ' pdf ' else ;
      end; // f_url_type

    procedure c_site_spider.test_filter_other_pages(p_c_strings: tStrings);
        // -- debg the selection

      procedure trace_filter(p_text: String);
        begin
          if m_trace_filter
            then display(p_text);
        end; // trace_filter

      procedure remove_selection_array_choices;

        function f_accept(p_c_url: c_url): Boolean;
          var l_page_level_type: t_page_level_type;
              l_page_type: t_page_type;
          begin
            Result:= True;
            trace_filter(p_c_url.m_name);
            for l_page_level_type:= e_parent_level to e_child_level do
              for l_page_type:= e_no_type to e_all_type do
                if m_selection_array[l_page_level_type, l_page_type]
                    and m_page_level_fo_array[l_page_level_type](p_c_url)
                  then begin
                      case l_page_type of
                        e_no_type : Result:= False;
                        e_all_type : Result:= True;
                        else Result:= m_page_type_fn_array[l_page_type](p_c_url);
                      end; // case

                      trace_filter('  '+ f_page_level_type_name(l_page_level_type)
                            + ' '+ f_page_type_name(l_page_type)
                            + ' '+ f_display_TF(Result));
                    end;
          end; // f_accept

        var l_remote_file_index: Integer;

        begin // remove_selection_array_choices
          with m_c_remote_file_list do
            for l_remote_file_index:= 0 to f_remote_file_count- 1 do
              with f_c_remote_file(l_remote_file_index) do
                if not m_downloaded (* and m_do_download *) and f_accept(m_c_normalized_url)
                  then p_c_strings.Add(m_name);
        end; // remove_selection_array_choices

      begin // test_filter_other_pages
        p_c_strings.Clear;
        remove_selection_array_choices;
      end; // test_filter_other_pages

    procedure c_site_spider.filter_other_pages;
        // -- remove unwanted pages

      procedure trace_filter(p_text: String);
        begin
          if m_trace_filter
            then display(p_text);
        end; // trace_filter

      procedure use_gui_filter;

        function f_accept(p_c_url: c_url): Boolean;
          var l_page_level_type: t_page_level_type;
              l_page_type: t_page_type;
          begin
            // Result:= True;
            // -- by default, no selection
            Result:= False;
            trace_filter(p_c_url.m_name);

            for l_page_level_type:= e_parent_level to e_child_level do
              for l_page_type:= e_no_type to e_all_type do
                if m_selection_array[l_page_level_type, l_page_type]
                    and m_page_level_fo_array[l_page_level_type](p_c_url)
                  then begin
                      case l_page_type of
                        e_no_type : Result:= False;
                        e_all_type : Result:= True;
                        else Result:= m_page_type_fn_array[l_page_type](p_c_url);
                      end; // case

                      trace_filter('gui_accept '+ f_page_level_type_name(l_page_level_type)
                            + ' '+ f_page_type_name(l_page_type)
                            + ' '+ f_display_TF(Result));
                    end;
          end; // f_accept

        var l_remote_file_index: Integer;

        begin // use_gui_filter
          display('> gui_filter');
          with m_c_remote_file_list do
            for l_remote_file_index:= 0 to f_remote_file_count- 1 do
              with f_c_remote_file(l_remote_file_index) do
                if not m_downloaded and m_do_download
                  then begin
                      m_do_download:= f_accept(m_c_normalized_url);
                      // -- add to the "other" list
                      if not m_do_download
                        then begin
                            display('gui_remove '+ m_name);
                            m_c_log_list.write_line(k_log_disabled_file, m_name);
                          end;
                    end;
          display('< gui_filter');
        end; // use_gui_filter

      procedure use_custom_callback_filter;
        var l_remote_file_index: Integer;
            l_accept: Boolean;
        begin
          display('> callback_filter');
          with m_c_remote_file_list do
          begin
            for l_remote_file_index:= 0 to f_remote_file_count- 1 do
              with f_c_remote_file(l_remote_file_index) do
                if not m_downloaded and m_do_download
                  then begin
                      m_on_filter_url(m_c_normalized_url, l_accept);
                      if not l_accept
                        then begin
                            m_do_download:= False;
                            // -- add to the "other" list
                            display('user_remove '+ m_name);
                            m_c_log_list.write_line(k_log_disabled_file, m_name);
                          end
                    end;
          end; // with m_c_remote_file_list
          display('< callback_filter');
        end; // use_custom_callback_filter

      procedure use_default_filter;
        var l_remote_file_index: Integer;
        begin
          display('> default_filter');
          with m_c_remote_file_list do
          begin
            for l_remote_file_index:= 0 to f_remote_file_count- 1 do
              with f_c_remote_file(l_remote_file_index) do
                if not m_downloaded and m_do_download
                  then begin
                      if f_is_page(m_c_normalized_url)
                        or
                         ( f_is_parent_page(m_c_normalized_url)
                         and
                           f_is_image_extension(m_c_normalized_url)
                         )
                        or
                         ( f_is_child_page(m_c_normalized_url)
                         and
                           f_is_html_extension(m_c_normalized_url)
                         )
                        then
                        else begin
                            m_do_download:= False;

                            if f_is_page(m_c_normalized_url)
                              then display('  page');
                            if  f_is_parent_page(m_c_normalized_url)
                                and f_is_image_extension(m_c_normalized_url)
                              then display('  parent_img');
                            if  f_is_child_page(m_c_normalized_url)
                                and f_is_html_extension(m_c_normalized_url)
                              then display('  child_html');

                            // -- add to the "other" list
                            display('default_remove '+ m_name);
                            m_c_log_list.write_line(k_log_disabled_file, m_name);
                          end
                    end;
          end; // with m_c_remote_file_list
          display('< default_filter');
        end; // use_default_filter

      begin // filter_other_pages
        display('> site.filter_other_pages');

        // -- first remove GUI choices
        if f_has_gui_selection
          then use_gui_filter
          else
            // -- then custom or inherited downloader choices
            if Assigned(m_on_filter_url)
              then use_custom_callback_filter
              else use_default_filter;

        display('< site.filter_other_pages');
      end; // filter_other_pages

    // -- select next page

    procedure c_site_spider.select_next_page_default(p_c_url: c_url; var pv_accept: Boolean);
      begin
        pv_accept:= f_is_page(p_c_url);
      end; // select_next_page_default

    procedure c_site_spider.select_next_page_default_child(p_c_url: c_url; var pv_accept: Boolean);
      begin
        pv_accept:= f_is_child_page(p_c_url) and f_is_html_extension(p_c_url);
      end; // select_next_page_default_child

    procedure c_site_spider.select_next_page_default_parent_images(p_c_url: c_url; var pv_accept: Boolean);
      begin
        pv_accept:= f_is_parent_page(p_c_url) and f_is_image_extension(p_c_url);
      end; // select_next_page_default_parent_images

    function c_site_spider.f_c_select_next_page: c_remote_file;
        // -- selects the next page. Change with user's strategy

      procedure find_gui_selection;

        procedure trace_selection(p_text: String);
          begin
            if m_trace_selection
              then display(p_text);
          end; // trace_selection

        function f_selected_in_level(p_page_level_type: t_page_level_type; p_c_url: c_url): Boolean;
          var l_page_type: t_page_type;
          begin
            Result:= False;

            for l_page_type:= e_no_type to e_all_type do
              if m_selection_array[p_page_level_type, l_page_type]
                  and m_page_level_fo_array[p_page_level_type](p_c_url)
                then begin
                    case l_page_type of
                      e_no_type : Result:= False;
                      e_all_type : Result:= True;
                      else Result:= m_page_type_fn_array[l_page_type](p_c_url);
                    end; // case

                    if Result
                      then begin
                          trace_selection(p_c_url.m_name
                              + ' | '+ f_url_type(p_c_url));
                          Exit;
                        end;
                  end;
          end; // f_selected_in_level

        var l_remote_file_index: Integer;

        begin // find_gui_selection
          display('> find_gui_selection');

          with m_c_remote_file_list do
          begin
            trace_selection('page');
            for l_remote_file_index:= 0 to f_remote_file_count- 1 do
              with f_c_remote_file(l_remote_file_index) do
                if not m_downloaded and m_do_download and (m_trial_count<= m_trial_count_max)
                  then begin
                      if f_selected_in_level(e_page_level, m_c_normalized_url)
                        then begin
                            Result:= f_c_self;
                            display('< find_gui_selection. Ok '+ f_c_self.m_name);
                            Exit;
                          end
                    end;

            trace_selection('child');
            for l_remote_file_index:= 0 to f_remote_file_count- 1 do
              with f_c_remote_file(l_remote_file_index) do
                if not m_downloaded and m_do_download and (m_trial_count<= m_trial_count_max)
                  then begin
                      if f_selected_in_level(e_child_level, m_c_normalized_url)
                        then begin
                            Result:= f_c_self;
                            display('< find_gui_selection. Ok '+ f_c_self.m_name);
                            Exit;
                          end
                    end;

            trace_selection('parent');
            for l_remote_file_index:= 0 to f_remote_file_count- 1 do
              with f_c_remote_file(l_remote_file_index) do
                if not m_downloaded and m_do_download and (m_trial_count<= m_trial_count_max)
                  then begin
                      if f_selected_in_level(e_parent_level, m_c_normalized_url)
                        then begin
                            Result:= f_c_self;
                            display('< find_gui_selection. Ok '+ f_c_self.m_name);
                            Exit;
                          end
                    end;
          end;

          display('< find_gui_selection. Not_found');
        end; // find_gui_selection

      procedure find_custom_or_default_selection;
        var l_selection_index: Integer;
            l_remote_file_index: Integer;
            l_accept: Boolean;
        begin
          with m_c_remote_file_list do
            for l_selection_index:= 0 to Length(m_oa_on_select_url)- 1 do
            begin
              for l_remote_file_index:= 0 to f_remote_file_count- 1 do
                with f_c_remote_file(l_remote_file_index) do
                  if not m_downloaded and m_do_download and (m_trial_count<= m_trial_count_max)
                    then begin
                        m_oa_on_select_url[l_selection_index](m_c_normalized_url, l_accept);
                        if l_accept
                          then begin
                              Result:= f_c_self;
                              Exit;
                            end
                      end;
            end; // with m_c_remote_file_list, for
        end; // find_custom_or_default_selection

      begin // f_c_select_next_page
        display('> site.f_c_select_next_page');
        Result:= Nil;

        // -- first gui choices
        if f_has_gui_selection
          then find_gui_selection
          else begin
              // -- if, on first path, had no custom selection,
              // --   then initialize the default selection
             if Length(m_oa_on_select_url)= 0
               then begin
                   append_selection_procedure(select_next_page_default);
                   append_selection_procedure(select_next_page_default_child);
                   append_selection_procedure(select_next_page_default_parent_images);
                 end;

             find_custom_or_default_selection;
           end;

        display('< site.f_c_select_next_page');
      end; // f_c_select_next_page

    Destructor c_site_spider.Destroy;
      begin
        Inherited;
      end; // Destroy

    begin // u_c_site_spider
    end. // u_c_site_spider


