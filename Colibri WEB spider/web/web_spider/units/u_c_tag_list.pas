// 002 u_c_tag_list
// 09 jun 2005

// -- (C) Felix John COLIBRI 2004
// -- documentation: http://www.felix-colibri.com

// -- tag list, AS IS

(*$r+*)

unit u_c_tag_list;
  interface
    uses Classes, u_c_basic_object
       , u_c_url
      ;

    type t_tag_type= (e_unknown_tag,
                       // -- "<A HREF="yyy.html" uuu>"
                       e_anchor_tag,

                       // -- <FRAME SRC= ...
                       e_frame_tag,
                       // -- <IMG ...
                       e_image_tag,

                     e_end_tag);
         t_set_of_tag_type= set of t_tag_type;

    type c_tag= // one "tag"
                Class(c_basic_object)
                  // -- m_name: the attributes
                  // -- index in the text (for anchor change)
                  m_text_index: Integer;
                  m_attributes_start_index: Integer;
                  m_attributes: string;

                  // -- anchor, frame, image
                  m_tag_type: t_tag_type;

                  // -- the tag as contained in the page
                  m_c_url: c_url;
                  // -- if no domain or segment, add the parent's
                  m_c_normalized_url: c_url;

                  Constructor create_tag(p_name: String;
                      p_text_index, p_attributes_start_index: Integer; p_attributes: string;
                      p_tag_type: t_tag_type;
                      p_c_url, p_c_normalized_url: c_url);
                  function f_c_self: c_tag;

                  function f_display_tag: String; Virtual;
                  procedure display_the_tag;
                  procedure display_tag_detail;
                  function f_display_tag_detail: String;
                  procedure deep_display_tag; Virtual;

                  Destructor Destroy; Override;
                end; // c_tag

         c_tag_list= // "tag" list
                     Class(c_basic_object)
                       m_c_tag_list: tStringList;

                       Constructor create_tag_list(p_name: String);

                       function f_tag_count: Integer;
                       function f_c_tag(p_tag_index: Integer): c_tag;

                       function f_c_add_tag(p_tag: String; p_c_tag: c_tag): c_tag;

                       function f_c_add_tag_element(p_tag: String;
                           p_text_index, p_attributes_start_index: Integer; p_attributes: string;
                           p_tag_type: t_tag_type;
                           p_c_url, p_c_normalized_url: c_url): c_tag;
                       procedure display_tag_list;
                       procedure display_the_tags;
                       procedure display_tag_detail;
                       procedure deep_display_tag_list;

                       procedure filter_and_display_the_tags(p_set_of_tag_type: t_set_of_tag_type;
                           p_page_name: String);
                       procedure filter_and_deep_display_tag_list(p_set_of_tag_type: t_set_of_tag_type;
                           p_page_name: String);

                       Destructor Destroy; Override;
                     end; // c_tag_list

    function f_tag_name(p_tag_type: t_tag_type): String;

  implementation
    uses SysUtils, u_types_constants, u_c_display, u_strings
      , u_dir, u_file
      ;

    // -- global

    function f_tag_name(p_tag_type: t_tag_type): String;
      begin
        Case p_tag_type of
          e_anchor_tag: Result:= 'A';
          e_frame_tag: Result:= 'FRAME';
          e_image_tag: Result:= 'IMG';
          else
            Result:= '?? ';
            display_bug_stop('tag_name ??_'+ IntToStr(Ord(p_tag_type)));
        end; // case
      end; // f_tag_name

    // -- c_tag

    Constructor c_tag.create_tag(p_name: String;
        p_text_index, p_attributes_start_index: Integer; p_attributes: string;
        p_tag_type: t_tag_type;
        p_c_url, p_c_normalized_url: c_url);
      begin
        Inherited create_basic_object(p_name);

        m_text_index:= p_text_index;
        m_attributes_start_index:= p_attributes_start_index;
        m_attributes:= p_attributes;
        m_tag_type:= p_tag_type;
        m_c_url:= p_c_url;
        m_c_normalized_url:= p_c_normalized_url;
        if m_c_normalized_url.m_domain= ''
          then display_bug_stop('c_tag.no_domain');
      end; // create_tag

    function c_tag.f_c_self: c_tag;
      begin
        Result:= Self;
      end; // f_c_self

    function c_tag.f_display_tag: String;
      begin
        Result:= Format('%-15s %5d ', [m_name, m_text_index]);
      end; // f_display_tag

    procedure c_tag.display_the_tag;
      begin
        display('  '+ f_tag_name(m_tag_type)+ ' '+ f_display_tag);
      end; // display_the_tag

    function c_tag.f_display_tag_detail: String;
      begin
        Result:= m_c_normalized_url.f_display_url_short;
        // Result:= '|'+ m_tag_prefix+ '|'+ m_tag_domain+ '|'+ m_tag_segment+ '|'+ m_tag_pure_file_name+ '|'+ m_tag_extension+ '|';
      end; // f_display_tag_detail

    procedure c_tag.display_tag_detail;
      begin
        display(f_display_tag_detail);
      end; // display_tag_detail

    procedure c_tag.deep_display_tag;
      begin
        display('      '+ m_name);
        display('        '+ IntToStr(m_text_index));
        // display('        '+ m_tag_start);
      end; // deep_display_tag

    Destructor c_tag.Destroy;
      begin
        m_c_url.Free;
        m_c_normalized_url.Free;
        
        InHerited;
      end; // Destroy

    // -- c_tag_list

    Constructor c_tag_list.create_tag_list(p_name: String);
      begin
        Inherited create_basic_object(p_name);

        m_c_tag_list:= tStringList.Create;
      end; // create_tag_list

    function c_tag_list.f_tag_count: Integer;
      begin
        Result:= m_c_tag_list.Count;
      end; // f_tag_count

    function c_tag_list.f_c_tag(p_tag_index: Integer): c_tag;
      begin
        Result:= c_tag(m_c_tag_list.Objects[p_tag_index]);
      end; // f_c_tag

    function c_tag_list.f_c_add_tag(p_tag: String; p_c_tag: c_tag): c_tag;
      begin
        Result:= p_c_tag;
        m_c_tag_list.AddObject(p_tag, Result);
      end; // f_c_add_tag

    function c_tag_list.f_c_add_tag_element(p_tag: String;
        p_text_index, p_attributes_start_index: Integer; p_attributes: string;
        p_tag_type: t_tag_type;
        p_c_url, p_c_normalized_url: c_url): c_tag;
      begin
        Result:= c_tag.create_tag(p_tag,
            p_text_index, p_attributes_start_index, p_attributes, p_tag_type, p_c_url, p_c_normalized_url);
        f_c_add_tag(p_tag, Result);
      end; // f_c_add_tag_element

    procedure c_tag_list.display_tag_list;
      var l_tag_index: Integer;
      begin
        display(m_name+ ' '+ IntToStr(f_tag_count));

        for l_tag_index:= 0 to f_tag_count- 1 do
          display(f_c_tag(l_tag_index).f_display_tag);
      end; // display_tag_list

    procedure c_tag_list.display_the_tags;
      var l_tag_index: Integer;
      begin
        for l_tag_index:= 0 to f_tag_count- 1 do
          f_c_tag(l_tag_index).display_the_tag;
      end; // display_the_tags

    procedure c_tag_list.display_tag_detail;
      var l_tag_index: Integer;
      begin
        display('    '+ IntToStr(f_tag_count));

        for l_tag_index:= 0 to f_tag_count- 1 do
          f_c_tag(l_tag_index).display_tag_detail;
      end; // display_tag_detail

    procedure c_tag_list.deep_display_tag_list;
      var l_tag_index: Integer;
      begin
        display('    '+ IntToStr(f_tag_count));

        for l_tag_index:= 0 to f_tag_count- 1 do
          f_c_tag(l_tag_index).deep_display_tag;
      end; // deep_display_tag_list

    // -- filter and display

    procedure c_tag_list.filter_and_display_the_tags(p_set_of_tag_type: t_set_of_tag_type;
        p_page_name: String);
      var l_tag_index: Integer;
      begin
        for l_tag_index:= 0 to f_tag_count- 1 do
          with f_c_tag(l_tag_index) do
            if m_tag_type in p_set_of_tag_type
              then begin
                  if p_page_name<> ''
                    then begin
                        display(p_page_name);
                        p_page_name:= '';
                      end;
                  display_the_tag;
                end;
      end; // filter_and_display_the_tags

    procedure c_tag_list.filter_and_deep_display_tag_list(p_set_of_tag_type: t_set_of_tag_type;
        p_page_name: String);
      var l_tag_index: Integer;
      begin
        for l_tag_index:= 0 to f_tag_count- 1 do
          with f_c_tag(l_tag_index) do
            if m_tag_type in p_set_of_tag_type
              then begin
                  if p_page_name<> ''
                    then begin
                        display(p_page_name);
                        p_page_name:= '';
                      end;
                  deep_display_tag;
                end;
      end; // filter_and_deep_display_tag_list

    Destructor c_tag_list.Destroy;
      var l_tag_index: Integer;
      begin
        for l_tag_index:= 0 to f_tag_count- 1 do
          f_c_tag(l_tag_index).Free;
        m_c_tag_list.Free;

        Inherited;
      end; // Destroy

    begin // u_c_tag_list
    end. // u_c_tag_list
