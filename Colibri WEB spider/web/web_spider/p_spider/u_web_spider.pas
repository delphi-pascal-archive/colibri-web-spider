// 003 u_web_spider
// 13 jun 2005

// -- (C) Felix John COLIBRI 2004
// -- documentation: http://www.felix-colibri.com

(*$r+*)

unit u_web_spider;
  interface
    uses Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
        StdCtrls
       , ComCtrls, ExtCtrls, FileCtrl
       , u_c_basic_object
       , u_c_remote_file_list_3
       , u_c_url
       , u_c_spider
       , u_c_site_spider, Buttons
       ;

    type TForm1= class(TForm)
    Panel1: TPanel;
    Panel3: TPanel;
    exit_: TButton;
    Panel4: TPanel;
    Panel5: TPanel;
    segment_label_: TLabel;
    page_label_: TLabel;
    extension_label_: TLabel;
    reception_panel_: TPanel;
    remaining_page_: TPanel;
    panel21: TPanel;
    Panel6: TPanel;
    PageControl1: TPageControl;
    other_pages_: TTabSheet;
    todo_pages_: TTabSheet;
    todo_pages_memo_: TMemo;
    running_pages_: TTabSheet;
    running_pages_memo_: TMemo;
    done_pages_: TTabSheet;
    done_pages_memo_: TMemo;
    http_: TTabSheet;
    http_memo_: TMemo;
    socket_: TTabSheet;
    Memo1: TMemo;
    domain_and_page_edit_: TEdit;
    requested_domain_label_: TLabel;
    requested_page_label_: TLabel;
    tags_: TTabSheet;
    tags_memo_: TMemo;
    current_bytes_label_: TLabel;
    total_bytes_label_: TLabel;
    client_count_: TPanel;
    Panel2: TPanel;
    trace_tag_: TCheckBox;
    clear_: TButton;
    active_panel_: TPanel;
    active_timer_: TTimer;
    dead_timer_: TTimer;
    main_path_panel_: TPanel;
    priority_segment_name_: TLabel;
    go_: TButton;
    requested_ip_label_: TLabel;
    extensions_: TTabSheet;
    Panel7: TPanel;
    Panel8: TPanel;
    Panel9: TPanel;
    remove_extension_memo_: TMemo;
    Panel10: TPanel;
    Panel11: TPanel;
    keep_extension_memo_: TMemo;
    pagecontrol3: TPageControl;
    test_: TTabSheet;
    decompose_: TButton;
    replace_image_tags_: TButton;
    save_page_list_: TButton;
    save_memos_: TButton;
    save_reception_buffer_: TButton;
    free_: TButton;
    strategy_: TTabSheet;

    parent_: TGroupBox;
    html_parent_: TSpeedButton;
    pdf_parent_: TSpeedButton;
    image_parent_: TSpeedButton;
    zip_parent_: TSpeedButton;
    child_: TGroupBox;
    html_child_: TSpeedButton;
    pdf_child_: TSpeedButton;
    image_child_: TSpeedButton;
    zip_child_: TSpeedButton;
    page_: TGroupBox;
    html_page_: TSpeedButton;
    pdf_page_: TSpeedButton;
    image_page_: TSpeedButton;
    zip_page_: TSpeedButton;
    no_parent_: TSpeedButton;
    all_parent_: TSpeedButton;
    no_page_: TSpeedButton;
    all_page_: TSpeedButton;
    no_child_: TSpeedButton;
    all_child_: TSpeedButton;
    test_other_: TButton;
                   procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure free_Click(Sender: TObject);
    procedure exit_Click(Sender: TObject);
    procedure save_page_list_Click(Sender: TObject);
    procedure save_memos_Click(Sender: TObject);
    procedure clear_Click(Sender: TObject);
    procedure active_timer_Timer(Sender: TObject);
    procedure dead_timer_Timer(Sender: TObject);
    procedure goClick(Sender: TObject);
    procedure decompose_Click(Sender: TObject);
    procedure replace_image_tags_Click(Sender: TObject);
    procedure clear__Click(Sender: TObject);
    procedure html_parent_Click(Sender: TObject);
    procedure test_other_Click(Sender: TObject);

                   private
                     procedure handle_before_download(p_c_spider: c_spider; p_c_remote_file: c_remote_file);
                     procedure handle_received_data(p_c_spider: c_spider; p_c_remote_file: c_remote_file);
                     procedure handle_after_save(p_c_spider: c_spider; p_c_remote_file: c_remote_file);

                     // -- selection procedures
                     procedure filter_page(p_c_url: c_url; var pv_accept: Boolean);
                     procedure select_next_page_child(p_c_url: c_url; var pv_accept: Boolean);
                     procedure select_next_page_parent_images(p_c_url: c_url; var pv_accept: Boolean);
                   public
                     procedure save_display_and_halt(p_text: String);
                     procedure handle_application_exception(Sender: TObject; E: Exception);

                 end;

    var Form1: TForm1;

  implementation
    uses WinSock

       , u_types_constants
       , u_c_display, u_c_log, u_display_hex_2
       , u_c_log_list
       , u_dir, u_file, u_exe
       , u_strings

       , u_c_tag_list
       , u_c_text_buffer
       , u_c_find_memo
       // , u_c_site_spider
       ;

    {$R *.DFM}


    const // k_url= 'http://www.martinfowler.com/index.html';
          // k_url= 'http://www.orafaq.com/articles/archives/2004_05.htm';
          // k_url= 'http://www.borland.com/index.html';
          // k_url= 'http://www.ronin-intl.com/company/scottAmbler.html';
          // k_url= 'http://www.inf.ethz.ch/personal/meyer/';
          // k_url= 'http://www.modelmakertools.com/';
          // k_url= 'http://www.modelmakertools.com/modelmaker/';
          // k_url= 'http://www.software-engineer.org/article_index.php?name=Software+Engineering+Process&category_id=6';
          // k_url= 'http://www.softpanorama.org/Algorithms/compilers.shtml';
          k_url= 'http://www.delphisources.ru/';


    var g_c_spider: c_site_spider= Nil;
        g_to_download_count, g_other_count, g_downloaded_count: Integer;
        g_current_byte_count, g_total_byte_count: Integer;
        g_c_other_pages_memo: c_find_memo= Nil;

    procedure clear_memos;
      begin
        with Form1 do
        begin
          g_c_other_pages_memo.m_c_memo.Lines.Clear;
          // -- keep the todo, in case wants to restart ?
          todo_pages_memo_.Lines.Clear;
          tags_memo_.Lines.Clear;
          running_pages_memo_.Lines.Clear;
          done_pages_memo_.Lines.Clear;
          http_memo_.Lines.Clear;

          // -- the socket and main display
          clear_display;
        end; // with Form1
      end; // clear_memos

    procedure set_color(p_color: Integer);
      begin
        Form1.client_count_.Color:= p_color;

        if p_color= clRed
          then Form1.active_timer_.Enabled:= True
          else
            if (p_color= clLime) or (p_color= clBlack)
              then begin
                  Form1.active_timer_.Enabled:= False;
                  Form1.dead_timer_.Enabled:= True;
                end
              else
                if p_color= clYellow
                  then begin
                      Form1.active_timer_.Enabled:= False;
                      Form1.dead_timer_.Enabled:= False;
                    end;

        Application.ProcessMessages;
      end; // set_color

    procedure TForm1.handle_before_download(p_c_spider: c_spider; p_c_remote_file: c_remote_file);
      var l_todo_pages_memo_index: Integer;
      begin
        display('> before_download');

        if requested_ip_label_.Caption= ''
          then requested_ip_label_.Caption:= p_c_spider.m_requested_ip;

        g_c_spider.m_c_log_list.write_line(k_log_running, '');
        g_c_spider.m_c_log_list.write_line(k_log_running, '  bef_down    '+ IntToStr(p_c_spider.m_save_level)+ ' '+ p_c_remote_file.m_name);
        // -- remove from the todo memo
        l_todo_pages_memo_index:= todo_pages_memo_.Lines.IndexOf(p_c_remote_file.m_name);
        if l_todo_pages_memo_index>= 0
          then begin
              g_c_spider.m_c_log_list.write_line(k_log_running, '- todo          '+ p_c_remote_file.m_name);
              todo_pages_memo_.Lines.Delete(l_todo_pages_memo_index)
            end
          else begin
              g_c_spider.m_c_log_list.write_line(k_log_running, '* todo          '+ p_c_remote_file.m_name);
              display_bug_halt('no_todo '+ p_c_remote_file.m_name);
            end;

        // -- add to the running memo
        g_c_spider.m_c_log_list.write_line(k_log_running, '+ add_running   '+ p_c_remote_file.m_name);
        running_pages_memo_.Lines.Add(p_c_remote_file.m_name);

        // -- the url display
        with p_c_remote_file.m_c_normalized_url do
        begin
          segment_label_.Caption:= m_segment;
          page_label_.Caption:= m_page;
          extension_label_.Caption:= m_extension;

          http_memo_.Lines.Add('GET '+ m_domain+  m_segment+ ' | '+ m_page+ m_extension);
        end; // with p_c_remote_file

        set_color(clRed);
        display('< before_download');
      end; // handle_before_download

    procedure tForm1.handle_received_data(p_c_spider: c_spider; p_c_remote_file: c_remote_file);
        // -- update byte counts
      var l_text: String;
          l_color: Integer;
      begin
        with p_c_remote_file.m_c_http_client_socket do
        begin
          Inc(g_current_byte_count, m_packet_bytes); Inc(g_total_byte_count, m_packet_bytes);

          if m_content_length= 0
            then begin
                l_text:= Format('%5d / %-5s', [g_current_byte_count, '-', g_total_byte_count]);
                l_color:= clRed;
            end
          else begin
              l_text:= Format('%5d / %-5d ', [m_downloaded_content_length, m_content_length]);
              if m_downloaded_content_length= m_content_length
                then l_color:= clLime
                else l_color:= clYellow;
            end;
        end; // with p_c_remote_file.m_c_http_client_socket

        // -- use a label to avoid
        with current_bytes_label_ do
        begin
          Caption:= l_text;
          Color:= l_color;
          // Application.ProcessMessages;
        end;

        total_bytes_label_.Caption:= Format('%5d', [g_total_byte_count]);
      end; // handle_received_data

    procedure tForm1.handle_after_save(p_c_spider: c_spider; p_c_remote_file: c_remote_file);
        // -- udpate running / done pages
        // -- also add new found pages
      var l_running_pages_memo_index: Integer;
          l_digit_count: Integer;
          l_size: String;
          l_404: String;

          l_remote_file_index: Integer;
      begin
        display('> handle_after_save');
        g_c_spider.m_c_log_list.write_line(k_log_running, '');
        g_c_spider.m_c_log_list.write_line(k_log_running, '  aft_save    '+ IntToStr(p_c_spider.m_save_level)+ ' '+ p_c_remote_file.m_name);
        // -- remove from the running memo
        l_running_pages_memo_index:= running_pages_memo_.Lines.IndexOf(p_c_remote_file.m_name);
        if l_running_pages_memo_index>= 0
          then begin
              g_c_spider.m_c_log_list.write_line(k_log_running, '- running       '+ p_c_remote_file.m_name);
              running_pages_memo_.Lines.Delete(l_running_pages_memo_index);
            end
          else begin
              g_c_spider.m_c_log_list.write_line(k_log_running, '* running       '+ p_c_remote_file.m_name);
              display_bug_halt('no_running '+ p_c_remote_file.m_name);
            end;

        // -- add to the done memo
        with p_c_remote_file, m_c_http_client_socket do
        begin
          if m_downloaded_content_length= 0
            then begin
                l_size:= 'z';
                l_digit_count:= 1;
              end
            else begin
                l_size:= f_display_k(13, m_downloaded_content_length);
                // -- count the digits
                l_digit_count:= Length(Trim(l_size))- 2- f_character_count(Trim(l_size), Chr(160));
              end;

          if m_answer_code= 404
            then l_404:= ' _404_'
            else l_404:= '      ';

          Form1.done_pages_memo_.Lines.Add(f_spaces(13- l_digit_count)
              + l_size+ '    '+ l_404+ '   '+ p_c_remote_file.m_name);
        end; // with p_c_remote_file, m_c_http_client_socket

        // -- go thru the list and add the new files
        display('remote_file_count '+ IntToStr(g_c_spider.m_c_remote_file_list.f_remote_file_count));

        with g_c_spider.m_c_remote_file_list do
          for l_remote_file_index:= 0 to f_remote_file_count- 1 do
            with f_c_remote_file(l_remote_file_index) do
              if not m_already_displayed
                then begin
                    if m_do_download
                      then begin
                          g_c_spider.m_c_log_list.write_line(k_log_running, '+ todo          '+ m_name);
                          todo_pages_memo_.Lines.Add(m_name);
                          Inc(g_to_download_count);
                        end
                      else begin
                          g_c_other_pages_memo.m_c_memo.Lines.Add(m_name);
                          Inc(g_other_count);
                        end;
                    m_already_displayed:= True;

                  end;
        Inc(g_downloaded_count);

        remaining_page_.Caption:=
            'other='+ IntToStr(g_other_count)
            + ', todo=' + IntToStr(g_to_download_count)
            + ', ok='+ IntToStr(g_downloaded_count);

        if g_to_download_count= g_downloaded_count
          then begin
              set_color(clLime);
            end;

        g_current_byte_count:= 0;

        display('< handle_after_save');
        Application.ProcessMessages;
      end; // handle_after_save

    // -- events

    procedure TForm1.FormCreate(Sender: TObject);
      begin
        remove_extension_memo_.Clear;
        remove_extension_memo_.Lines.Add('.avi');
        remove_extension_memo_.Lines.Add('.mov');
        remove_extension_memo_.Lines.Add('.mpg');
        remove_extension_memo_.Lines.Add('.mp3');
        remove_extension_memo_.Lines.Add('.zip');

        keep_extension_memo_.Clear;
        keep_extension_memo_.Lines.Add('.gif');
        keep_extension_memo_.Lines.Add('.png');
        keep_extension_memo_.Lines.Add('.jpg');
        keep_extension_memo_.Lines.Add('.jpeg');

        initialize_display(Memo1.Lines);
        g_c_display.m_display_line_max:= 1024;
        g_c_display.m_display_indentation_max:= 64;

        initialize_default_log;

        domain_and_page_edit_.Text:= k_url;

(*
        Application.OnException:= handle_application_exception;
*)

        g_c_other_pages_memo:= c_find_memo.create_find_memo('other_pages',
            other_pages_, f_exe_path+ 'log\');

        clear_memos;
        // g_c_other_pages_memo.m_c_memo.Lines.LoadFromFile('..\web_spider\p_spider\u_web_spider.pas');

        set_color(clYellow);
      end; // FormCreate

    procedure TForm1.handle_application_exception(Sender: TObject; E: Exception);
      begin
        save_display_and_halt('*** exception '+ E.Message+ '. Has_saved_page_list. HALT');
(*

        save_page_list_Click(Nil);
        save_memos_Click(Nil);
        // save_reception_buffer_Click(Nil);
        display('*** exception '+ E.Message+ '. Has_saved_page_list. HALT');

        Halt;

        Application.ShowException(E);
        Application.Terminate;
*)
      end; // handle_application_exception

    procedure TForm1.clear_Click(Sender: TObject);
      begin
        clear_display;
        // -- also other memos ?
      end; // clear_Click

    procedure TForm1.exit_Click(Sender: TObject);
      begin
        Close;
      end; // close_Click

    // -- start spider

    procedure TForm1.filter_page(p_c_url: c_url; var pv_accept: Boolean);
      begin
        display('> filter_page '+ p_c_url.m_name);
        with g_c_spider do
          // pv_accept:= f_is_child_page(p_c_url) and f_is_html_extension(p_c_url);
(*
          pv_accept:= (f_is_child_page(p_c_url) and f_is_html_extension(p_c_url))
              or
                   ( f_is_domain_page(p_c_url) and not f_is_child_page(p_c_url)
                     and (p_c_url.m_extension= '.png')
                   );
*)
          pv_accept:= (p_c_url.m_extension= '.pdf') or (p_c_url.m_extension= '.ps');
        display('< filter_page '+ f_display_TF(pv_accept));
      end; // filter_page

    procedure TForm1.select_next_page_child(p_c_url: c_url; var pv_accept: Boolean);
      begin
        display('> select_next_page_child '+ p_c_url.m_name);
(*

        with g_c_spider do
          pv_accept:= f_is_child_page(p_c_url) and f_is_html_extension(p_c_url);
*)
        display('< select_next_page_child '+ f_display_TF(pv_accept));
      end; // select_next_page_child

    procedure TForm1.select_next_page_parent_images(p_c_url: c_url; var pv_accept: Boolean);
      begin
        display('> select_next_page_parent_images '+ p_c_url.m_name);
(*
        with g_c_spider do
          pv_accept:= f_is_domain_page(p_c_url) and not f_is_child_page(p_c_url)
              and (p_c_url.m_extension= '.png');
*)
        display('< select_next_page_parent_images '+ f_display_TF(pv_accept));
      end; // select_next_page_parent_images

    procedure TForm1.goClick(Sender: TObject);

      procedure add_gui_selection;
        begin
          with g_c_spider do
          begin
            set_selection(e_parent_level, e_no_type, no_parent_.Down);
            set_selection(e_parent_level, e_html_type, html_parent_.Down);
            set_selection(e_parent_level, e_image_type, image_parent_.Down);
            set_selection(e_parent_level, e_zip_type, zip_parent_.Down);
            set_selection(e_parent_level, e_pdf_type, pdf_parent_.Down);
            set_selection(e_parent_level, e_all_type, all_parent_.Down);

            set_selection(e_page_level, e_no_type, no_page_.Down);
            set_selection(e_page_level, e_html_type, html_page_.Down);
            set_selection(e_page_level, e_image_type, image_page_.Down);
            set_selection(e_page_level, e_zip_type, zip_page_.Down);
            set_selection(e_page_level, e_pdf_type, pdf_page_.Down);
            set_selection(e_page_level, e_all_type, all_page_.Down);

            set_selection(e_child_level, e_no_type, no_child_.Down);
            set_selection(e_child_level, e_html_type, html_child_.Down);
            set_selection(e_child_level, e_image_type, image_child_.Down);
            set_selection(e_child_level, e_zip_type, zip_child_.Down);
            set_selection(e_child_level, e_pdf_type, pdf_child_.Down);
            set_selection(e_child_level, e_all_type, all_child_.Down);
          end; // with_g_c_spider  
        end; // add_gui_selection

      var l_c_url: c_url;

      begin // goClick
        l_c_url:= c_url.create_url(domain_and_page_edit_.Text);

        with l_c_url do
        begin
          f_decompose_url;
          display(f_display_url);

          if (m_protocol<> '') and (m_protocol<> 'http://')
            then begin
                Free;
                Exit;
              end;

          if m_protocol= ''
            then begin
                m_protocol:= 'http://';
                m_name:= m_protocol+ m_domain+ m_segment+ m_page+ m_extension;
                domain_and_page_edit_.Text:= m_name;
              end;
          if m_segment= ''
            then begin
                m_segment:= '/';
                m_name:= m_protocol+ m_domain+ m_segment+ m_page+ m_extension;
                domain_and_page_edit_.Text:= m_name;
              end;

          // -- update the controls
          clear_memos;
          requested_ip_label_.Caption:= '';
        end; // with l_c_url

        if not DirectoryExists(f_exe_path+ '..\_download\')
          then check_path_and_name(f_exe_path+ '..\_download\');
        g_to_download_count:= 0; g_other_count:= 0; g_downloaded_count:= 0;
        g_current_byte_count:= 0; g_total_byte_count:= 0;

        display('create_spider');
        // -- create a spider and add this page
        g_c_spider.Free;
        g_c_spider:= c_site_spider.create_site_spider('spider', f_exe_path+ '..\_download\', l_c_url);

        display('link');
        with g_c_spider, m_c_remote_file_list do
        begin
          requested_domain_label_.Caption:= m_c_requested_url.m_domain;
          requested_page_label_.Caption:= m_c_requested_url.m_page;
          priority_segment_name_.Caption:= m_c_requested_url.m_segment;

          todo_pages_memo_.Lines.Clear;
          Inc(g_to_download_count);

          m_on_before_download:= handle_before_download;
          m_on_received_data:= handle_received_data;
          m_on_after_save:= handle_after_save;
          m_c_remove_extension_list.Assign(remove_extension_memo_.Lines);
          m_c_keep_extension_list.Assign(keep_extension_memo_.Lines);

(*
          m_on_filter_url:= filter_page;
          append_selection_procedure(select_next_page_child);
          append_selection_procedure(select_next_page_parent_images);
*)
          add_gui_selection;

          m_trace_filter:= True;
          m_trace_selection:= true;

          display('go_download');
          if f_remote_file_count> 0
            then begin
                todo_pages_memo_.Lines.Add(f_c_remote_file(0).m_name);
                download_the_file(f_c_remote_file(0));
              end;
        end; // with g_c_spider
      end; // goClick

    procedure TForm1.save_page_list_Click(Sender: TObject);
      begin
        display('> save_page_list');

        if g_c_spider<> Nil
          then with g_c_spider do
            if m_c_remote_file_list<> Nil
              then m_c_remote_file_list.save_to_file(m_root_save_path+ m_c_requested_url.m_domain+ '\_log\save_file_list.txt');

        display('< save_page_list');
      end; // save_page_list_Click

    // -- free

    procedure TForm1.free_Click(Sender: TObject);
      // -- also called by OnClose
      begin
        g_c_spider.Free;
        g_c_spider:= Nil;

        g_c_display.Free;
        g_c_display:= Nil;
      end; // free_Click

    procedure TForm1.save_display_and_halt(p_text: String);
      begin
        save_page_list_Click(Nil);
        save_memos_Click(Nil);

        display(p_text);
        ShowMessage(p_text);
        Halt;

        Application.Terminate;
      end; // save_display_and_halt

    procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
      begin
        save_memos_Click(Nil);
        free_Click(Nil);
      end; // FormClose

    procedure TForm1.save_memos_Click(Sender: TObject);
      begin
        display('> save_page_list');

(*
        with other_pages_memo_.Lines do
          if Count<> 0
            then SaveToFile(g_root_save_path+ g_domain_name+ '\_log\save_other_pages.txt');
        with todo_pages_memo_.Lines do
          if Count<> 0
            then SaveToFile(g_root_save_path+ g_domain_name+ '\_log\save_todo_pages.txt');
        with http_memo_.Lines do
          if Count<> 0
            then SaveToFile(g_root_save_path+ g_domain_name+ '\_log\save_http.txt');
        with tags_memo_.Lines do
          if Count<> 0
            then SaveToFile(g_root_save_path+ g_domain_name+ '\_log\save_tags.txt');
*)
        display('< save_memos_Click');
      end; // save_memos_Click

    procedure TForm1.active_timer_Timer(Sender: TObject);
        // -- while running, blink to display activity
      begin
        if active_panel_.Color= clRed
          then active_panel_.Color:= clLime
          else active_panel_.Color:= clRed;
      end; // active_timer_Timer

    procedure TForm1.dead_timer_Timer(Sender: TObject);
      begin
        active_timer_.Enabled:= False;
        client_count_.Color:= clBlack;
      end; // dead_timer_Timer

    procedure TForm1.decompose_Click(Sender: TObject);
      begin
        PageControl1.ActivePage:= socket_;
        with c_url.create_url(domain_and_page_edit_.Text) do
        begin
          m_display_tag_analysis:= True;
          f_decompose_url;
          display(f_display_url);
          Free;
        end; // with c_url
      end; // decompose_Click

    procedure TForm1.replace_image_tags_Click(Sender: TObject);
      begin
        PageControl1.ActivePage:= socket_;
        clear_display;

        if g_c_spider<> Nil
          then g_c_spider.replace_image_tags;
      end; // replace_image_tags_Click

    procedure TForm1.clear__Click(Sender: TObject);
      begin
        g_c_other_pages_memo.m_c_memo.Lines.Clear;
      end; // clear__Click

    procedure TForm1.html_parent_Click(Sender: TObject);
      var l_page_level_type: t_page_level_type;
          l_page_type: t_page_type;
          l_display: String;
      begin
        display('> selection_Click');
        with Sender as tSpeedButton do
        begin
          if Parent= parent_
            then l_page_level_type:= e_parent_level else
          if Parent= page_
            then l_page_level_type:= e_page_level else
          if Parent= child_
            then l_page_level_type:= e_child_level else
            l_page_level_type:= e_unknown_level;

          if Caption= 'no'
            then l_page_type:= e_no_type else
          if Caption= 'html'
            then l_page_type:= e_html_type else
          if Caption= 'img'
            then l_page_type:= e_image_type else
          if Caption= 'zip'
            then l_page_type:= e_zip_type else
          if Caption= 'pdf'
            then l_page_type:= e_pdf_type else
          if Caption= 'all'
            then l_page_type:= e_all_type else
            l_page_type:= e_unknown_type;

          l_display:= f_page_level_type_name(l_page_level_type)+ ' '
              + f_page_type_name(l_page_type);

          if Down
            then display('add    '+ l_display)
            else display('remove '+ l_display);

          if g_c_spider<> Nil
            then begin
                g_c_spider.set_selection(l_page_level_type,
                    l_page_type, Down);
                g_c_spider.display_selection;
              end;
        end; // with Sender
        display('< selection_Click');
      end; // html_parent_Click

    procedure TForm1.test_other_Click(Sender: TObject);
        // -- debug: test the gui on the "other" pages
      begin
        if g_c_spider<> nil
          then
            with g_c_spider do
            begin
              if f_has_gui_selection
                then display('has_selection');
              test_filter_other_pages(g_c_other_pages_memo.m_c_memo.Lines);
            end;
      end; // test_other_Click

end.

