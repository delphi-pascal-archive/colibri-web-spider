// 003 u_c_url
// 13 jun 2005

// -- (C) Felix John COLIBRI 2004
// -- documentation: http://www.felix-colibri.com

// -- decompose the url

(*$r+*)

unit u_c_url;
  interface
    uses u_c_basic_object;

    type c_url= class(c_basic_object)
                  // -- m_name: the url
                  m_protocol, m_domain, m_segment, m_page, m_extension, m_cgi_parameters, m_target: String;

                  m_display_tag_analysis: Boolean;
                  m_decomposition_error: Boolean;

                  Constructor create_url(p_name: String);

                  function f_decompose_url: Boolean;

                  function f_display_url: String;
                  function f_display_url_short: String;
                  function f_is_html_extension: Boolean;

                  function f_c_clone: c_url;
                  function f_c_normalized_url: c_url;
                  function f_c_dos_save_names: c_url;

                  Destructor Destroy; Override;
                end; // c_url

    function f_refuse_all(p_c_url: c_url): boolean;
    function f_is_html_extension(p_c_url: c_url): boolean;
    function f_is_image_extension(p_c_url: c_url): boolean;
    function f_is_zip_extension(p_c_url: c_url): boolean;
    function f_is_pdf_extension(p_c_url: c_url): boolean;
    function f_accept_all(p_c_url: c_url): boolean;

  implementation
    uses SysUtils, u_c_display
        , u_characters, u_strings, u_dir;

    // -- page checks - extension type

    function f_refuse_all(p_c_url: c_url): boolean;
      begin
        Result:= False;
      end; // f_refuse_all

    function f_is_html_extension(p_c_url: c_url): boolean;
      begin
        Result:= p_c_url.f_is_html_extension
      end; // f_is_html_extension

    function f_is_image_extension(p_c_url: c_url): boolean;
      var l_extension: String;
      begin
        with p_c_url do
          l_extension:= LowerCase(m_extension);
        Result:= (l_extension= '.png')
               or (l_extension= '.gif')
               or (l_extension= '.jpg')
               or (l_extension= '.jpeg')
              ;
      end; // f_is_image_extension

    function f_is_zip_extension(p_c_url: c_url): boolean;
      var l_extension: String;
      begin
        with p_c_url do
          l_extension:= LowerCase(m_extension);
        Result:= (l_extension= '.zip')
               or (l_extension= '.rar')
               or (l_extension= '.bnz2')
      end; // f_is_zip_extension

    function f_is_pdf_extension(p_c_url: c_url): boolean;
      var l_extension: String;
      begin
        with p_c_url do
          l_extension:= LowerCase(m_extension);
        Result:= (l_extension= '.pdf')
               or (l_extension= '.ps')
               or (l_extension= '.gz')
      end; // f_is_pdf_extension

    function f_accept_all(p_c_url: c_url): boolean;
      begin
        Result:= True;
      end; // f_accept_all

    // -- c_url

    Constructor c_url.create_url(p_name: String);
        // -- k_url= 'http://staff.cs.utu.fi/kurssit/Programming-III/101.html'
      begin
        Inherited create_basic_object(p_name);
      end; // create_url

    function c_url.f_display_url: String;
      begin
        Result:= '( '+ m_protocol+ ' | '+ m_domain+ ' ) '+ m_segment+ ' | '+ m_page+ ' | '+ m_extension;
        if (m_cgi_parameters<> '') or (m_target<> '')
          then Result:= Result+ ' [ '+ m_cgi_parameters+ ' | '+ m_target+ ' ] ';
      end; // f_display_url

    function c_url.f_display_url_short: String;
      begin
        Result:= '('+ m_protocol+ '|'+ m_domain+ ')'+ m_segment+ '|'+ m_page+ '|'+ m_extension;
        if (m_cgi_parameters<> '') or (m_target<> '')
          then Result:= Result+ '['+ m_cgi_parameters+ '|'+ m_target+ ']';
      end; // f_display_url_short

    function c_url.f_decompose_url: Boolean;
      // -- after "KEY ="
      // -- everything up to next " or space
      // --  - protocol whatever is before ":" (http or mailto)
      var l_explain: String;

      var l_index, l_length, l_start_index: Integer;
          l_last_slash_index, l_diese_index, l_dot_index, l_colon_index: Integer;
          l_third_slash_index: Integer;

      procedure trace_decompose(p_text: String);
        begin
          if m_display_tag_analysis
            then display(p_text);
        end; // trace_decompose

      procedure display_error(p_text: String);
        begin
          display_bug_stop(p_text);
          m_decomposition_error:= True;
        end; // display_error

      procedure compute_indexes;
        var // -- because < aaa/bbb.html/#goto: last slash is not a "path / file" separator
            l_previous_slash_index: Integer;
            // -- for "http://xxxx.yyy.zzz/..."
            l_slash_count: Integer;
        begin
          trace_decompose('> compute_indexes |'+ m_name+ '|');
          m_protocol:= ''; m_domain:= '';
          m_segment:= ''; m_page:= ''; m_extension:= ''; m_target:= '';

          l_index:= 1;
          l_length:= Length(m_name);
          l_start_index:= l_index;
          l_last_slash_index:= 0; l_previous_slash_index:= 0;
          l_diese_index:= 0;
          l_colon_index:= 0;
          l_dot_index:= 0;

          l_slash_count:= 0;
          l_third_slash_index:= 0;

          // -- analyze until ", > or blanks
          while l_index<= l_length do
          begin
            case m_name[l_index] of
              '/' : begin
                      l_previous_slash_index:= l_last_slash_index;
                      l_last_slash_index:= l_index;
                      Inc(l_slash_count);
                      if l_slash_count= 3
                        then l_third_slash_index:= l_index;
                      // -- could have " href = "//www.cs.fsu.edu/~awang/courses/cis6935_s2004">CIS "
                      // -- => cancel the dot
                      l_dot_index:= 0;
                    end;
              '#' : l_diese_index:= l_index;
              '.' : begin
                      // -- found a dot: remember the position of the extension start
                      // --  - skip ..
                      // --  - skip dot after #  "<A href="chap6.htm#6.1">"
                      // -- - todo : "./xxx"
                      if (l_index+ 1<= l_length) and (m_name[l_index+ 1]= '.')
                          or (l_diese_index<> 0)
                        then
                             Inc(l_index)
                        else l_dot_index:= l_index;
                    end;
              ':' : begin
                      if (l_dot_index= 0)
                        then l_colon_index:= l_index;
                    end;
            end; // case

            Inc(l_index);
          end; // while

          // check_error(f_extract_string_start_end_and_check(l_start_index, l_index- 1, l_tag_start),
          //    'm_tag_start', l_tag_start);

          // -- if <A HREF="rep\fichier.html\#goto">, the last path \ is then n-1'th \
          if (l_diese_index> 0) and (l_last_slash_index= l_diese_index- 1)
            then begin
                if m_display_tag_analysis
                  then display('adjust slash');
                l_last_slash_index:= l_previous_slash_index;
              end;

          trace_decompose(Format(': %2d . %2d / %2d # %2d',
              [l_colon_index, l_dot_index, l_last_slash_index, l_diese_index]));
          trace_decompose('< compute_indexes');
        end; // compute_indexes

      procedure check_error(p_ok: Boolean; p_error_text: String; var pv_result: String);
        begin
          if not p_ok
            then begin
                // display_error(p_error_text+ pv_result);
                pv_result:= '';
              end;
        end; // check_error

      procedure extract_starting_protocol_and_domain;
        var l_colon_slash_slash: String;
            l_last_domain_index: Integer;
        begin
          trace_decompose('> protocol_and_domain');
          // -- protocol: "mailto:xxx" or  "http://www..."
          // --   but NOT "c:\prog"
          if l_colon_index> 0
            then check_error(f_extract_string_and_check(m_name, l_start_index, l_colon_index- 1, m_protocol),
                    'protocol', m_protocol);

          // if (m_protocol<> '') and (LowerCase(m_protocol)<> m_protocol)
          //  then display_error('protocol_not_lower '+ m_protocol);

          // display('protocol '+ m_protocol);

          if m_protocol= 'mailto'
            then begin
                l_explain:= 'mailto';
                m_protocol:= m_protocol+ ':';
                Result:= False;
              end
            else
              if m_protocol= 'http'
                then begin
                    // -- "http:// path + page + diese "
                    // --
                    l_colon_slash_slash:= f_extract_string(m_name, l_colon_index, l_colon_index+ 2);
                    if l_colon_slash_slash= '://'
                      then begin
                          m_protocol:= m_protocol+ l_colon_slash_slash;
                          l_start_index:= l_colon_index+ 3;
                          // -- get the domain: everything up to the next /
                          if l_third_slash_index<> 0
                            then l_last_domain_index:= l_third_slash_index- 1
                            else // -- no ending / : "http://xxx.yyy.zzz"
                                 l_last_domain_index:= l_index- 1;

                          check_error(f_extract_string_and_check(m_name, l_start_index, l_last_domain_index, m_domain),
                               'domain ', m_domain);

                          // -- correct the last slash index
                          // --  if no path: "http://visit.webhosting.yahoo.com/visit.gif"
                          l_start_index:= l_last_domain_index+ 1;
                          if l_dot_index< l_start_index
                            then l_dot_index:= 0;
                          // -- "http://www.martinfowler.com"
                          if l_last_slash_index= l_colon_index+ 2
                            then l_last_slash_index:= 0;
                        end
                      else begin
                          l_explain:= 'http_no_://';
                          Result:= False;
                        end;
                  end
                else begin
                    // -- ftp://, https://
                    Result:= m_protocol= '';
                  end;
          trace_decompose('< protocol_and_domain');
        end; // extract_starting_protocol_and_domain

      procedure extract_segment;
          // -- if "href="/back.gif">" is absolute path
        var l_segment: String;
        begin
          trace_decompose('> segment');
          // -- if "http://visit.webhosting.yahoo.com/visit.gif", then segment= "/"
          // -- a path ?
          if l_last_slash_index> 0
            then begin
                trace_decompose('last_/_ix '+ IntToStr(l_last_slash_index));
                // check_error(f_extract_string_and_check(m_name, l_start_index, l_last_slash_index- 1, m_segment),
                //    'path ', m_segment);
                check_error(f_extract_string_and_check(m_name, l_start_index, l_last_slash_index, l_segment),
                    'path ', l_segment);

                trace_decompose('last_/_ix '+ IntToStr(l_last_slash_index)+ '"'+ l_segment+ '"');

                // -- always end with a slash
                if l_segment<> ''
                  then begin
                      l_segment:= f_replace_character(l_segment, '/', '\');
                      l_segment:= f_with_ending_slash(l_segment);

                      // -- do NOT add starting slash: relative path, like "../../xxx"
                      l_segment:= f_replace_character(l_segment, '\', '/');
                    end;
                m_segment:= l_segment;

                // if (m_segment<> '') and (m_segment<> LowerCase(m_segment))
                //   then display_error('path_not_lower'+ m_segment);

                l_start_index:= l_last_slash_index+ 1;
                // display('path '+ m_segment);
              end;
          trace_decompose('< segment |'+ m_segment+ '|');
        end; // extract_segment

      var l_file_end_index: Integer;

      procedure extract_ending_target;
        begin
          // -- now remove target from end
          // -- get the terminationg target, if any "#xxx"
          if l_diese_index> 0
            then begin
                check_error(f_extract_string_and_check(m_name, l_diese_index+ 1, l_index- 1, m_target),
                    'target_name ', m_target);

                l_file_end_index:= l_diese_index;
                // -- <A HREF="rep\fichier.html\#goto">
                if (l_diese_index> 1) and (m_name[l_diese_index- 1] in ['/', '\'])
                  then Dec(l_file_end_index);
              end
            else begin
                l_file_end_index:= l_index;
              end;
        end; // extract_ending_target

      procedure extract_middle_page_name;
        begin
          trace_decompose('> page_name');
          // -- in the middle remains the file_name+ extension
          // -- get the terminating extension, if any
          if l_dot_index> 0
            then begin
                check_error(f_extract_string_and_check(m_name, l_dot_index+ 1, l_file_end_index- 1, m_extension),
                    'extension ', m_extension);
                // if (m_extension<> '') and (m_extension<> LowerCase(m_extension))
                //   then display_error('lower_extension '+ m_extension);

                l_file_end_index:= l_dot_index;
              end;

          // -- the remaining file name
          check_error(f_extract_string_and_check(m_name, l_start_index, l_file_end_index- 1, m_page),
              'file_name ', m_page);

          // -- "http://www.martinfowler.com/bliki"
          // -- "bliki"
          if (m_page<> '') and (l_dot_index= 0)
              and
                 ( (l_last_slash_index> 0)
                  or
                   (l_last_slash_index= 0) and (m_domain= '')
                 )
            then begin
                // -- the "page" is in fact the segment
                if m_segment= ''
                  then m_segment:= '/';
                m_segment:= m_segment+ m_page+ '/';
                m_page:= '';
              end;
          trace_decompose('< page_name |'+ m_page+ '|'+ m_segment+ '|');
        end; // extract_middle_page_name

      begin // f_decompose_url
        Result:= True;
        l_explain:= '';

        compute_indexes;

        if Result
          then begin
              extract_starting_protocol_and_domain;

              if Result
                then begin
                    extract_segment;
                    extract_ending_target;
                    extract_middle_page_name;
                  end; // path_file_name
            end;

        if m_extension<> ''
          then m_extension:= '.'+ m_extension;

        trace_decompose('|'+ m_protocol+ '|'+ m_domain+ '|'+ m_segment+ '|'+ m_page+ '|'+ m_extension+ '|'+ m_target+ '|');
      end; // f_decompose_url

    function c_url.f_is_html_extension: Boolean;
      var l_extension: String;
      begin
        l_extension:= Lowercase(m_extension);
        Result:= (l_extension= '.htm') or (l_extension= '.html')
           or (l_extension= '.shtml')
           or (l_extension= '.aspx') or (l_extension= '.asp')
      end; // f_is_html_extension

    function c_url.f_c_clone: c_url;
      begin
        Result:= c_url.create_url(m_name);

        Result.m_protocol:= m_protocol;
        Result.m_domain:= m_domain;
        Result.m_segment:= m_segment;
        Result.m_page:= m_page;
        Result.m_extension:= m_extension;
        Result.m_cgi_parameters:= m_cgi_parameters;
        Result.m_target:= m_target;
      end; // f_c_clone

    function c_url.f_c_normalized_url: c_url;
      var l_no_percent_name: String;
      begin
        l_no_percent_name:= f_replace_percent_nn(m_name);
        Result:= c_url.create_url(l_no_percent_name);
        Result.f_decompose_url;

        // -- ?? also remove the spaces after / ?
      end; // f_c_normalized_url

    function c_url.f_c_dos_save_names: c_url;
        // -- clones from the normalized url => no %nn
        // -- change / to \, remove ? *, no "." path, no "." file
      begin
        Result:= c_url.create_url(m_name);

        Result.m_domain:= m_domain;
        Result.m_segment:= f_replace_character(m_segment, '/', '\');

        if m_page= ''
          then Result.m_page:= 'no_name'
          else begin
              Result.m_page:= m_page;
              Result.m_page:= f_replace_character_set_not_in(m_page, (k_letters+ k_digits), '_');
            end;

        if m_extension= ''
          then // -- will decide later if "_.txt" or "_.html"
          else begin
              Result.m_extension:= m_extension;
              // -- check ".letters_digits
              with Result do
                if m_extension[1]= '.'
                  then begin
                      Delete(m_extension, 1, 1);
                      if f_contains_only(m_extension, k_letters+ k_digits+ ['_'])
                        then m_extension:= '.'+ m_extension
                        else begin
                            m_extension:= '.'+ f_replace_character_set_not_in(m_extension, (k_letters+ k_digits), '_');
                          end;
                    end
                  else begin
                      m_extension:= '.'+ f_replace_character_set_not_in(m_extension, (k_letters+ k_digits), '_');
                    end
            end;
      end; // f_c_dos_save_names

    Destructor c_url.Destroy;
      begin
        Inherited;
      end; // Destroy

    begin // u_c_url
    end. // u_c_url


