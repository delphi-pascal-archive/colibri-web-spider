// 002 u_c_remote_html_file_3
// 09 jun 2005

// -- (C) Felix John COLIBRI 2004
// -- documentation: http://www.felix-colibri.com

// -- an html_remote_file
// --   has the anchor tag analysis

// -- analysis: find < and next >
// --  - no comment analysis
// --  - no script analysis
// --  - no "xxx<yyy" analysis

(*$r+*)

unit u_c_remote_html_file_3;
  interface
    uses Classes, u_c_basic_object
      , u_c_log
      , u_c_url
      , u_c_tag_list
      , u_c_text_buffer
      , u_c_remote_file_list_3
      ;

    type c_remote_html_file= // c_remote_html_file
                             Class(c_remote_file)
                               m_c_text_buffer: c_text_buffer;

                               m_c_tag_list: c_tag_list;

                               m_trace_tag_analysis: Boolean;
                               m_display_tag_analysis: Boolean;
                               m_c_tag_analysis_strings_ref: tStrings;

                               Constructor create_remote_html_file(p_c_url, p_c_normalized_url: c_url);
                               function f_c_self: c_remote_html_file;

                               procedure copy_data_to_c_text_buffer(p_pt: Pointer; p_length: Integer);

                               procedure analyze_text_buffer(p_c_text_buffer: c_text_buffer;
                                   p_c_log_all_tag, p_c_log_tag_analysis: c_log);
                               procedure load_from_file(p_path, p_file_name: String);

                               procedure replace_image_tags_in_html;

                               Destructor Destroy; Override;
                             end; // c_remote_html_file

  implementation
    uses SysUtils, Math, u_types_constants, u_c_display, u_strings, u_characters
      , u_dir, u_file
      , u_c_path_segments
      , u_c_spider // the log_list constants
      ;

    // -- c_remote_html_file

    Constructor c_remote_html_file.create_remote_html_file(p_c_url, p_c_normalized_url: c_url);
      begin
        Inherited create_remote_file(p_c_url, p_c_normalized_url);

        m_c_tag_list:= c_tag_list.create_tag_list('tag_list');
      end; // create_remote_html_file

    function c_remote_html_file.f_c_self: c_remote_html_file;
      begin
        Result:= Self;
      end; // f_c_self

    procedure c_remote_html_file.copy_data_to_c_text_buffer(p_pt: Pointer; p_length: Integer);
      begin
        m_c_text_buffer:= c_text_buffer.create_text_buffer('main', p_length);
        Move(p_pt^, m_c_text_buffer.m_oa_text_buffer[0], p_length);
      end; // copy_data_to_c_text_buffer

    procedure c_remote_html_file.analyze_text_buffer(p_c_text_buffer: c_text_buffer;
        p_c_log_all_tag, p_c_log_tag_analysis: c_log);
        // -- keep at file level, to incorporate root pathes,
        // -- and for better handling of html errors
      var l_error_count: Integer;

      // -- trace and error

      procedure trace_tag_analysis(p_text: String);
        begin
          if m_trace_tag_analysis
            then
              if m_c_tag_analysis_strings_ref<> Nil
                then m_c_tag_analysis_strings_ref.Add(p_text)
                else display(p_text);
        end; // trace_tag_analysis

      procedure display_error(p_text: String);
        begin
          // display_bug_stop(p_text);
          display('tag_analysis_err: '+ p_text+ '<');
          Inc(l_error_count);
        end; // display_error

      procedure check_error(p_ok: Boolean; p_error_text: String; var pv_result: String);
        begin
          if not p_ok
            then begin
                display_error(p_error_text+ pv_result);
                pv_result:= '';
              end;
        end; // check_error

      // -- common to all tags

      var l_tag_start_index: Integer;

      function f_get_tag_type: t_tag_type;
          // -- has found "<". Analyze the following string
        var l_start_index: Integer;
            l_tag_name: String;
        begin
          with p_c_text_buffer do
          begin
            l_tag_name:= '';

            l_start_index:= m_buffer_index;
            while (m_buffer_index< m_buffer_size) and (m_oa_text_buffer[m_buffer_index] in k_letters) do
              Inc(m_buffer_index);

            if m_buffer_index= l_start_index
              then Result:= e_unknown_tag
              else begin
                  l_tag_name:= LowerCase(f_extract_string_start_end(l_start_index, m_buffer_index- 1));
                  if l_tag_name= 'a'
                    then Result:= e_anchor_tag
                    else
                      if l_tag_name= 'frame'
                        then Result:= e_frame_tag
                        else
                          if l_tag_name= 'img'
                            then Result:= e_image_tag
                            else Result:= e_unknown_tag;
                end;
          end; // with p_c_text_buffer
        end; // f_get_tag_type

      procedure analyze_the_tag(p_tag_type: t_tag_type);

        procedure skip_blanks_returns;
          begin
            with p_c_text_buffer do
              while (m_buffer_index< m_buffer_size) and (m_oa_text_buffer[m_buffer_index] in k_blanks_returns) do
                Inc(m_buffer_index);
          end; // skip_blanks_returns

        function f_skip_equal: Boolean;
          begin
            with p_c_text_buffer do
            begin
              skip_blanks_returns;

              Result:= (m_buffer_index< m_buffer_size)
                  and (m_oa_text_buffer[m_buffer_index]= '=');

              If Result
                then Inc(m_buffer_index);

              // -- here should be on " or '
            end; // with p_c_text_buffer
          end; // f_skip_equal

        // -- extract

        function f_get_letters: String;
          var l_start_index: Integer;
          begin
            with p_c_text_buffer do
            begin
              skip_blanks_returns;

              l_start_index:= m_buffer_index;
              while (m_buffer_index< m_buffer_size)
                  and (m_oa_text_buffer[m_buffer_index] in k_letters) do
                Inc(m_buffer_index);
              if m_buffer_index= l_start_index
                then Result:= ''
                else Result:= f_extract_string_start_end(l_start_index, m_buffer_index- 1);
            end; // with p_c_text_buffer
          end; // f_get_letters

        function f_find_string_in_tag(p_text: String): Boolean;
            // -- skip until finds this string or >
            // -- case INSENSITIVE
          var l_letters: String;
          begin
            Result:= False;
            with p_c_text_buffer do
            begin
              repeat
                skip_blanks_returns;
                l_letters:= f_get_letters;
                // display(l_letters);
                if LowerCase(l_letters)= LowerCase(p_text)
                  then begin
                       Result:= True;
                       Exit;
                     end;
                if (m_buffer_index>= m_buffer_size) or (m_oa_text_buffer[m_buffer_index]= '>')
                  then Exit;
                Inc(m_buffer_index);
              until False;
            end; // with p_c_text_buffer
          end; // f_find_string_in_tag

        var l_attributes_start_index: Integer;

        function f_extract_attributes: String;
          var l_ending_character: Char;
              l_start_index: Integer;
          begin
            with p_c_text_buffer do
            begin
              // -- skip the blanks after = (should be none)
              skip_blanks_returns;

              // -- skip ", if any

              l_ending_character:= m_oa_text_buffer[m_buffer_index];

              if m_oa_text_buffer[m_buffer_index] in  ['"', '''']
                then Inc(m_buffer_index)
                else begin
                    display('no_starting_" '+ IntToStr(m_buffer_index)+ ' '
                        + f_extract_string_start_end(Max(0, m_buffer_index- 10), m_buffer_index));
                    l_ending_character:= chr(0);
                  end;

              l_start_index:= m_buffer_index;
              l_attributes_start_index:= m_buffer_index;

              while (m_buffer_index< m_buffer_size)
                  // and not (m_oa_text_buffer[m_buffer_index] in (k_blanks_returns+ ['"', '>'])) do
                  and not (m_oa_text_buffer[m_buffer_index] in (k_blanks_returns+ [l_ending_character, '>'])) do
                Inc(m_buffer_index);

              if m_buffer_index>= m_buffer_size
                then Result:= 'TO_END'
                else Result:= f_extract_string_start_end(l_start_index, m_buffer_index- 1);
            end; // with p_c_text_buffer
          end; // f_extract_attributes

        function f_build_c_urls(var pv_attributes: String;
            var pv_c_url, pv_c_normalized_url: c_url): Boolean;
          var l_dos_segment: String;
          begin
            Result:= True;

            pv_attributes:= f_extract_attributes;
            if pv_attributes<> ''
              then begin
                  pv_c_url:= c_url.create_url(pv_attributes);

                  if not pv_c_url.f_decompose_url
                    then begin
                        display('url_nok '+ pv_c_url.f_display_url_short);
                        if m_c_parent_remote_file_list= Nil
                          then display_bug_halt('rem_parent');
                        if m_c_parent_remote_file_list.m_c_log_list_ref= Nil
                          then display_bug_halt('rem_log_list');
                        with m_c_parent_remote_file_list.m_c_log_list_ref.f_find_by_c_log(k_log_tags_nok) do
                          write_line(pv_attributes+ ' '+ pv_c_url.f_display_url_short);
                        Result:= False;
                        pv_c_url.Free;
                      end
                    else begin
                        // -- remove %nn
                        pv_c_normalized_url:= pv_c_url.f_c_normalized_url;
                        // -- if relative, add parent's domain and segments
                        with pv_c_normalized_url do
                        begin
                          if m_protocol= ''
                            then m_protocol:= 'http://';

                          if m_domain= ''
                            then begin
                                // stop('dom >'+ Self.m_c_normalized_url.m_domain+ '<');
                                // -- but if "uuu.html" must add domain and segment
                                m_domain:= Self.m_c_normalized_url.m_domain;

                                if m_segment= ''
                                  then m_segment:= Self.m_c_normalized_url.m_segment
                                  else
                                    if m_segment[1]= '/'
                                      then
                                      else m_segment:= Self.m_c_normalized_url.m_segment+ '/'+ m_segment;
                              end
                            else begin
                                // -- if "http://a.b.c/xxx.html" the segment is really empty
                              end;

                          // -- remove ..
                          if m_segment<> ''
                            then begin
                                l_dos_segment:= f_replace_character(m_segment, '/', '\');

                                if Pos('..', l_dos_segment)> 0
                                  then l_dos_segment:= f_remove_middle_dot_dot(l_dos_segment);
                                if (Pos('.\', l_dos_segment)> 0) or (Pos('\.', l_dos_segment)> 0)
                                  then l_dos_segment:= f_remove_middle_dot(l_dos_segment);

                                // -- end with a single \
                                if l_dos_segment<> ''
                                  then l_dos_segment:= f_with_ending_slash(l_dos_segment);
                                // -- remove starting \
                                l_dos_segment:= f_with_one_slash_at_beginning(l_dos_segment);

                                if (Pos('..', m_segment)> 0) and (Pos('..', l_dos_segment)> 0)
                                  then stop(pv_attributes+ ' ' );
                                // -- back to HTTP
                                m_segment:= f_replace_character(l_dos_segment, '\', '/');
                              end;

                          // -- make sure that the m_name contains the modified values
                          m_name:= m_protocol+ m_domain+ m_segment+ m_page+ m_extension;
                        end; // with l_c_normalized_url
                      end;
                  end
                else Result:= False;
          end; // f_build_c_urls

        // -- specific tags

        procedure extract_A_tag;
            // -- <A HREF="files.html">
            // -- <A HREF="path\files.html">

            // -- <A HREF="#goto">
            // -- <A HREF="rep\file.html\#goto">
            // -- ds: <A HREF="rep\file.html#goto"> (no / before #)

            // -- <A HREF="mailto:x.y@a.b">
            // -- <A HREF="http://rep/file.html">
            // -- "href" not in first parameter:
            // -- <a class='tocTitle' href="documentation/business/datatypes.html">Jaffa's Data Types</a>

            // -- <A NAME=goto>
            // -- <A NAME="goto">

            // -- <a style="text-decoration:none"
            // --    onmouseover="ion('iN670','http://www.ulb.ac.be/design/toc1a.gif',
            // --       'http://www.ulb.ac.be/design/toc1.gif')"
            // --    onmouseout="ioff()" href="tp1.pdf">my_bla_bla</a>,
          var l_key_word: String;
              // l_start_index: Integer;
              l_attributes: String;
              l_c_url, l_c_normalized_url: c_url;
          begin
            trace_tag_analysis('');
            trace_tag_analysis('> extract_A_tag ');

            l_key_word:= LowerCase(f_get_letters);
            l_attributes:= '';

            if l_key_word= 'href'
              then begin
                  if f_skip_equal and f_build_c_urls(l_attributes, l_c_url, l_c_normalized_url)
                    then begin
                        m_c_tag_list.f_c_add_tag_element(l_c_normalized_url.m_name,
                            l_tag_start_index, l_attributes_start_index, l_attributes,
                            e_anchor_tag, l_c_url, l_c_normalized_url);

                        p_c_log_tag_analysis.write_line(l_attributes+ ' '+ l_c_normalized_url.f_display_url_short);
                        if l_c_normalized_url.m_domain= ''
                          then p_c_log_tag_analysis.write_line('  dom '+ Self.m_c_normalized_url.m_domain+ '<');
                      end
                    else trace_tag_analysis('not_A ');
                end
              else
                if l_key_word= 'name'
                  then trace_tag_analysis('NAME')
                  else begin
                      // -- HREF not in first position, but somewhere later (javascript ?)
                      if f_find_string_in_tag('href') and f_skip_equal
                          and f_build_c_urls(l_attributes, l_c_url, l_c_normalized_url)
                        then begin
                            m_c_tag_list.f_c_add_tag_element(l_c_normalized_url.m_name,
                                l_tag_start_index, l_attributes_start_index, l_attributes,
                                e_anchor_tag, l_c_url, l_c_normalized_url);
                          end
                        else display_error('A_not_NAME_or_HREF');
                    end;

            trace_tag_analysis('< extract_A_tag %'+ l_attributes+ '%');
          end; // extract_A_tag

        procedure extract_IMG_tag;
            // -- <IMG SRC="ip4.jpeg">
            // -- <img id="Image733" src="/Patterns/Diagrams/flyweight.gif" border="0" />
            // -- if "href="/back.gif">" is absolute path
          var l_attributes: String;
              l_c_url, l_c_normalized_url: c_url;
          begin
            trace_tag_analysis('');
            trace_tag_analysis('> extract_IMG_tag');
            l_attributes:= '';

            if (f_find_string_in_tag('src')) and f_skip_equal
                and f_build_c_urls(l_attributes, l_c_url, l_c_normalized_url)
                // and f_analyze_attributeand f_built_key_and_dos_segment
              then begin
                  m_c_tag_list.f_c_add_tag_element(l_c_normalized_url.m_name,
                      l_tag_start_index, l_attributes_start_index, l_attributes,
                      e_image_tag, l_c_url, l_c_normalized_url);
                end
            else display_error('no_IMG_SRC');

            trace_tag_analysis('< extract_IMG_tag %'+ l_attributes+ '%');
          end; // extract_IMG_tag

        procedure extract_FRAME_tag;
            // -- <FRAME SRC="edito_top.html" SCROLLING="no" FRAMEBORDER="0">
            // -- <FRAME NAME="left" SRC="newleft.html" SCROLLING="No" FRAMEBORDER="No" NORESIZE>
          var l_attributes: String;
              l_c_url, l_c_normalized_url: c_url;
          begin
            trace_tag_analysis('');
            trace_tag_analysis('> extract_FRAME_tag');

            l_attributes:= '';

            // -- find 'src'
            if (f_find_string_in_tag('src')) and f_skip_equal
                and f_build_c_urls(l_attributes, l_c_url, l_c_normalized_url)
              then begin
                  // -- "<FRAME SRC="menu/top.html" NAME="topUML" SCROLL etc> "
                  m_c_tag_list.f_c_add_tag_element(l_c_normalized_url.m_name,
                      l_tag_start_index, l_attributes_start_index, l_attributes,
                      e_frame_tag, l_c_url, l_c_normalized_url);
                end;

            trace_tag_analysis('< extract_FRAME_tag %'+ l_attributes+ '%');
          end; // extract_FRAME_tag

        begin // analyze_the_tag
          case p_tag_type of
            e_anchor_tag : extract_A_tag;
            e_frame_tag : extract_FRAME_tag;
            e_image_tag : extract_IMG_tag;
            else
              // -- m_buffer_index is beyond "<" : do nothing else
              // --   comment, script... ?
          end; // case
        end; // analyze_the_tag

      var l_A_count, l_anti_A_count: Integer;
          l_tag_type: t_tag_type;

      begin // analyze_text_buffer
        display('> analyze_text_buffer '+ IntToStr(p_c_text_buffer.m_buffer_size));

        p_c_log_all_tag.write_line('new   '+ m_name);

        p_c_log_tag_analysis.write_line('');
        p_c_log_tag_analysis.write_line('tags_of '+ m_c_normalized_url.f_display_url_short);

        with p_c_text_buffer do
        begin
          l_A_count:= 0; l_anti_A_count:= 0;

          while m_buffer_index< m_buffer_size do
          begin
            if (m_oa_text_buffer[m_buffer_index]= '<')
              then begin
                  l_tag_start_index:= m_buffer_index;

                  Inc(m_buffer_index);

                  l_tag_type:= f_get_tag_type;
                  analyze_the_tag(l_tag_type);
                end
              else Inc(m_buffer_index);
          end; // while m_buffer_index

          if l_A_count<> l_anti_A_count
            then begin
                // display_once(l_file_name, 'A ='+ IntTOStr(l_A_count)+ '  /A '+ IntToStr(l_anti_A_count));
                // Inc(m_error_count);
              end;

          display('< analyze_text_buffer ix= '+ IntToStr(m_buffer_index)+ ' A='+ IntToStr(l_A_count));
        end; // with p_c_text_buffer
      end; // analyze_text_buffer

    procedure c_remote_html_file.load_from_file(p_path, p_file_name: String);
        // -- load a page from disk (to anlayze its tags)
      begin
        m_c_text_buffer:= c_text_buffer.create_text_buffer('txt_buffer', 0);

        with m_c_text_buffer do
        begin
          if f_load_from_file(p_path+ p_file_name)
            then analyze_text_buffer(m_c_text_buffer, nil, nil)
            else display_bug_stop('*** no '+ p_path+ p_file_name);

          Free;
        end; // with l_c_text_buffer
      end; // load_from_file

    procedure c_remote_html_file.replace_image_tags_in_html;
      var l_c_text_buffer: c_text_buffer;
          l_buffer_size: Integer;

      procedure do_replace(p_c_image_tag: c_tag);
        var l_remote_file_index: Integer;
            l_new_name: String;
        begin
          with p_c_image_tag do
          begin
            display(m_name);
            l_remote_file_index:= m_c_parent_remote_file_list.f_index_of(m_name);
            display('  '+ IntToStr(l_remote_file_index));

            if l_remote_file_index>= 0
              then
                with m_c_parent_remote_file_list.f_c_remote_file(l_remote_file_index) do
                begin
                  if m_downloaded
                    then begin
                        display('    '+ m_saved_name);
                        l_new_name:= ExtractFileName(m_saved_name);
                        display('    '+ IntToStr(m_text_index)+ '|'+ m_attributes
                            + '|'+ l_new_name);
                        if l_c_text_buffer.f_check_string(m_attributes_start_index, m_attributes)
                          then l_c_text_buffer.replace_string(m_attributes_start_index, m_attributes, l_new_name)
                          else display('*** delta ');
                        Inc(l_buffer_size, Length(l_new_name)- Length(m_attributes));
                      end;

                end;
          end; // with p_c_image_tag
        end; // do_replace

      var l_tag_index: Integer;
          l_new_save_name: String;

      begin // replace_image_tags_in_html
        l_c_text_buffer:= c_text_buffer.create_text_buffer('buffer', 0);
        with l_c_text_buffer do
        begin
          display('reload '+ m_saved_name+ '|');
          if not f_load_from_file(m_saved_name)
            then display_bug_halt('no_reload '+ m_saved_name);
          // -- make some room
          l_buffer_size:= m_buffer_size;
          SetLength(l_c_text_buffer.m_oa_text_buffer, l_buffer_size* 2);
          // Inc(m_buffer_size);
          display('max '+ IntToStr(Length(m_oa_text_buffer)));
        end; // with l_c_text_buffer

        with m_c_tag_list do
        begin
          for l_tag_index:= f_tag_count- 1 downto 0 do
            with f_c_tag(l_tag_index) do
              if m_tag_type= e_image_tag
                then begin
                    do_replace(f_c_self);
                  end;
        end; // with m_c_tag_list

        with l_c_text_buffer do
        begin
          // -- adjust the size of the text
          l_new_save_name:= ExtractFilePath(m_saved_name)+ '_'+ ExtractFileName(m_saved_name);
          display('save as '+ l_new_save_name);
          save_to_file(l_new_save_name);
          Free;
        end; // with l_c_text_buffer
      end; // replace_image_tags_in_html

    Destructor c_remote_html_file.Destroy;
      begin
        m_c_text_buffer.Free;
        m_c_tag_list.Free;

        Inherited;
      end; // Destroy

    begin // u_c_tag_list
    end. // u_c_tag_list
