*&---------------------------------------------------------------------*
*& Report ZKS_MISC_PRG
*&---------------------------------------------------------------------*
report zks_misc_prg.

*cl_demo_output=>display( 'x' ).



PARAMETERS: p_url1 TYPE swc_value DEFAULT 'https://github.com',
            p_url2 TYPE swc_value DEFAULT 'https://api.github.com',
            p_id   TYPE ssfapplssl DEFAULT 'ANONYM'.
* api.github.com is used when pushing code back to github

SELECTION-SCREEN BEGIN OF BLOCK proxy WITH FRAME.
* proxy settings, fill if your system is behind a proxy
PARAMETERS: p_proxy  TYPE string,
            p_pxport TYPE string,
            p_puser  TYPE string,
            p_ppwd   TYPE string.
SELECTION-SCREEN END OF BLOCK proxy.

START-OF-SELECTION.
  PERFORM run USING p_url1.
  WRITE: /, '----', /.
  PERFORM run USING p_url2.

FORM run USING iv_url TYPE swc_value.

  DATA: lv_code          TYPE i,
        lv_url           TYPE string,
        li_client        TYPE REF TO if_http_client,
        lt_errors        TYPE TABLE OF string,
        lv_error_message TYPE string.

  IF iv_url IS INITIAL.
    RETURN.
  ENDIF.

  lv_url = iv_url.
  cl_http_client=>create_by_url(
    EXPORTING
      url           = lv_url
      ssl_id        = p_id
      proxy_host    = p_proxy
      proxy_service = p_pxport
    IMPORTING
      client        = li_client ).

  IF NOT p_puser IS INITIAL.
    li_client->authenticate(
      proxy_authentication = abap_true
      username             = p_puser
      password             = p_ppwd ).
  ENDIF.

  li_client->send( ).
  li_client->receive(
    EXCEPTIONS
      http_communication_failure = 1
      http_invalid_state         = 2
      http_processing_failed     = 3
      OTHERS                     = 4 ).
  IF sy-subrc <> 0.
    WRITE: / 'Error Number', sy-subrc, /.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    li_client->get_last_error(
      IMPORTING
        message = lv_error_message ).
    SPLIT lv_error_message AT cl_abap_char_utilities=>newline INTO TABLE lt_errors.
    LOOP AT lt_errors INTO lv_error_message.
      WRITE: / lv_error_message.
    ENDLOOP.
    WRITE: / 'Also check transaction SMICM -> Goto -> Trace File -> Display End'.
    RETURN.
  ENDIF.

* if SSL Handshake fails, make sure to also check https://launchpad.support.sap.com/#/notes/510007

  li_client->response->get_status(
    IMPORTING
      code = lv_code ).
  IF lv_code = 200.
    WRITE: / lv_url, ': ok'.
  ELSE.
    WRITE: / 'Error', lv_code.
  ENDIF.

ENDFORM.



*************************
* Consume OData-Service *
*************************
*parameters input type string lower case.
**lv_service = 'https://lb-fd0.sap.comp.db.de/sap/opu/odata/sap/ZKS_TEST_DD_CDS/zks_test_dd?$format=json&$top=1'.
**data(lv_service) = conv string( 'https://lb-fd0.sap.comp.db.de/sap/opu/odata/sap/ZKS_TEST_DD_CDS/zks_test_dd(%27100112%27)/gjahr/$value' ).
*
*
**** Use CL_HTTP_CLIENT to consume the OData service using the method "create_by_url"
*cl_http_client=>create_by_url(
*     exporting
*       url                = input
**       url                = lv_service
*     importing
*       client             = data(lo_http_client)
*).
*
**** Call the following method to autheticate the user/password and client for the remote connection.
*lo_http_client->authenticate(
*    client   = '903'
*    username = 'D0000326286'
*    password = 'Btgzhn77242!'
*).
*
**** Send the request
*lo_http_client->send( ).
*
**** Receive the response
*lo_http_client->receive( ).
*
**** Read and display the result
*cl_demo_output=>display( lo_http_client->response->get_cdata( ) ).
*
*
*at selection-screen output.
*  loop at screen.
*    if screen-name = 'INPUT'.
*      screen-invisible = 1.
*      modify screen.
*    endif.
*  endloop.



**************************************
* SALV_IDA                           *
* Darstellung von Datenbank-Tabellen *
**************************************
*cl_salv_gui_table_ida=>create( iv_table_name = 'ZMI_PS_GM03_V' )->fullscreen( )->display( ).



************************
* Dialog Datei-Auswahl *
************************
*data lt_file type filetable.
*data lv_rc   type i.
*cl_gui_frontend_services=>file_open_dialog(
*  exporting
*    window_title            =     'Bitte Datei auswählen !'
*  changing
*    file_table              =     lt_file
*    rc                      =     lv_rc ).



**********************************************************************
* Werte formatieren: Länge, Ausrichtung, Werte vorne/hinten anhängen *
* String Templates, Overlay                                          *
**********************************************************************
*DATA(x) = 123.
*WRITE:  / |{ x     ALIGN = RIGHT  WIDTH = 10  PAD = '0' }|.
*WRITE:  / |{ 123   ALIGN = LEFT   WIDTH = 10  PAD = '0' }|.
*WRITE:  / |{ 'kai' ALIGN = RIGHT  WIDTH = 10  PAD = '0' }|.
*DATA(a) = 100.
*DATA(b) = |{ a ALIGN = RIGHT  WIDTH =  6  PAD = '0' }|.
*DATA(c) = |{ b ALIGN = RIGHT  WIDTH = 16  PAD = '+' }|.
*WRITE:  / c.
*
* Overlay (Werte hinten anhängen)
*DATA    y(6) TYPE c VALUE '1'.
*OVERLAY y WITH 'xxxxxx'.
*WRITE:  / y.



********
* SALV *
********
*select * from marc into table @data(lt_marc).
*
*cl_salv_table=>factory( importing r_salv_table = data(lr_salv_table) " Inline-Deklaration der Instanz
*                        changing  t_table      = lt_marc ).
*
*lr_salv_table->display( ).



******************************************
* Über Komponenten einer Struktur loopen *
******************************************
* S. ZKS_ANF_73314_PRG



****************************************************************
* Interne Tabelle befüllen und Zeichen zu Nummern konvertieren *
****************************************************************
*DATA x TYPE TABLE OF c.
*SPLIT '1 2 3' AT ' ' INTO TABLE x.
*cl_demo_output=>display( ( x[ 1 ] + 2 ) / x[ 2 ] ).



********************************
* Tabelle mit Tabelle als Feld *
********************************
*DATA line TYPE i.
*DATA data TYPE TABLE OF i.
*TYPES: BEGIN OF test,
*         line_nr LIKE line,
*         values  LIKE data,
*       END OF test.
*DATA lv_test TYPE test.
*lv_test-line_nr = 1.
*DATA(value) = 1000.
*APPEND value TO lv_test-values.
*
*cl_demo_output=>display( lv_test-values[ 1 ] ).



*****************************************************************
* Dereferenzierung -> Typisierte Struktur in generische Tabelle *
* APPEND CORRESPONDING #( line ) TO itab.                       *
*****************************************************************
*FIELD-SYMBOLS <results> TYPE table.
*DATA empty TYPE zmi_ps_vn.
*
*SELECT * FROM zmi_ps_vn INTO TABLE @DATA(results_1) WHERE finve = '00227' AND gjahr = '2002'.
*SELECT * FROM zmi_ps_vn INTO TABLE @DATA(results_2) WHERE finve = '00227' AND gjahr = '2002' AND zzplktp = ''.
*
*DATA ref_new TYPE REF TO data.
*CREATE DATA ref_new TYPE TABLE OF zmi_ps_vn.
*ASSIGN ref_new->* TO <results>.
*
*LOOP AT results_1 INTO DATA(tab_line).
*  READ TABLE results_2 WITH KEY posid = tab_line-posid zuord = tab_line-zuord ASSIGNING FIELD-SYMBOL(<line>).
*  IF sy-subrc <> 0.
**    MOVE-CORRESPONDING tab_line to empty.
**    empty-zuord '9999'.
**    APPEND empty TO <results>.
*    APPEND CORRESPONDING zmi_ps_vn( tab_line ) TO <results>. "Entspricht dem Auskommentierten
*  ELSE.
*    APPEND <line> TO <results>.
*  ENDIF.
*ENDLOOP.
*
*cl_demo_output=>display( <results> ).



************************************************************
* VALUE-Operator: DB- und interne Tabellen befüllen Teil 2 *
************************************************************
*" 1. Datenbanktabelle befüllen
*insert zcoa from @( value #( coa_user_id = 7 coa_username = 'Username 1' coa_dienstort = 60528 zage = 10 ) ).
*" Test
*select * from zcoa into table @data(x).
*cl_demo_output=>display( x ).
*
*" 2. Interne Tabelle befüllen
*" a. Tabellentyp mit generischem Datentyp selber deklarieren
*types lty_int type table of i with empty key.
*data(lt_int) = value lty_int( ( 1 ) ( 2 ) ( 3 ) ).
*" b. Mit bestehendem (Integer-)Tabellentyp
*data(lt_int) = value UJCTRL_T_INTEGER( ( 1 ) ( 2 ) ( 3 ) ).
*" Test
*cl_demo_output=>display( lt_int ).



*********
* RANGE *
*********
*
*" 1. Deklaration über Datentyp und klassische Befüllung
*DATA lt_range TYPE RANGE OF i.
*DATA ls_range LIKE LINE OF lt_range.
*ls_range-low    = '1'.
*ls_range-high   = '100'.
*ls_range-sign   = 'I'.
*ls_range-option = 'BT'.
*APPEND ls_range TO lt_range.
*
*" 2. Deklaration über Datentyp mit TYPES und Befüllung über VALUES
*TYPES lr_range TYPE RANGE OF i.
*DATA(lr_range) = VALUE lr_range( ( low = '101' high = '200' sign = 'I' option = 'BT' ) ).
*
*" 3. Deklaration über Datenelement und Befüllung mit VALUES
*DATA lr_range TYPE RANGE OF db2sint.
*lr_range = VALUE #( ( low = '101' high = '200' sign = 'I' option = 'BT' ) ).
*
*" Test
*PARAMETERS: my_par TYPE i.
*IF my_par IN lr_range.
*  cl_demo_output=>display( 'In range' ).
*ELSE.
*  cl_demo_output=>display( 'Not in range' ).
*ENDIF.



***************************************
* Datum zu Integer, Rechnen mit Datum *
* Shortcut für Substrings             *
***************************************
*DATA(x) = sy-datum+0(4) - 2000. " +0 ist Offset/Startpunkt, (4) die Länge des Substrings
*WRITE x.



*********************
* Messages aus T100 *
*********************
*" se16n -> t100 -> Passenden Text suchen
*" Message: Der Gültigkeitsbeginn muß einem Jahresbeginn entsprechen
*MESSAGE ID 'ZMICOSTA1' TYPE 'I' NUMBER 119.
*MESSAGE i119(zmicosta1).



***********************
* SELECT mit GROUP BY *
***********************
*DATA(year) = sy-datum(4) - 1.
*DATA lt_zmi_ps_fbef_n TYPE TABLE OF zmi_ps_fbef_n.
*
*SELECT finkz,
*       gjahr,
*       SUM( fkzbtr_nf ) AS fkzbtr_nf,
*       mandt,
*       kokrs,
*       bukrs
*  FROM zmi_ps_fbef_n
*  WHERE gjahr <= @year
*  ""Alle SELECT-Parameter, die keine Aggregatfunktion haben, müssen im GROUP BY auftauchen
*  GROUP BY finkz, gjahr, bukrs, mandt, kokrs, bukrs
*  ORDER BY finkz, gjahr
*  INTO CORRESPONDING FIELDS OF TABLE @lt_zmi_ps_fbef_n
*  UP TO 30 ROWS.
*
*cl_demo_output=>display( lt_zmi_ps_fbef_n ).
*
*LOOP AT lt_zmi_ps_fbef_n ASSIGNING FIELD-SYMBOL(<z>).
*  WRITE / <z>-finkz.
*ENDLOOP.



***************************************
* Compute factorial recursively       *
* Parameters with modified input text *
* Exception class-based               *
***************************************
*parameters input type i. " Text modified under Goto -> Text Elements -> Selection Texts
*data(lo_zcl_misc) = new zcl_misc( ).
*try.
*    data(factorial) = lo_zcl_misc->compute_factorial_recursively( input ).
*    cl_demo_output=>display( factorial ).
*  catch cx_demo_exception.
*    cl_demo_output=>display('No numbers below 0 allowed!').
*endtry.



**************************************
* Int8 as equivalent to Long in Java *
**************************************
*data x type int8 value 9223372036854775807.



**********************************************************
* Interne Tabelle mit Struktur einer DB-Tabelle als Feld *
* READ TABLE                                             *
**********************************************************
*" Struktur mit Struktur von zcoa als Feld
*types: begin of struct,
*         id     type i,
*         it_coa type zcoa,
*       end of struct.
*
*data itab type table of struct.
*data line_itab type struct.
*" Tabellenzeile befüllen und anhängen
*line_itab-id = 1.
*select single * from zcoa into line_itab-it_coa.
*append line_itab to itab.
*
*" Diese Zeile entspricht itab[ 1 ]
*read table itab index 1 assigning field-symbol(<fs>).
*" Auslesen 1: Mit write nur Unterfeld möglich
*write <fs>-it_coa-coa_username.
*" Auslesen 2: Mit demo_output auch ganzes Strukturfeld möglich.
*" itab[ 1 ] entspricht read table ... index ... assigning field-symbol
*cl_demo_output=>display( itab[ 1 ]-it_coa ).



*****************************************
* Function call with CHANGING parameter *
*****************************************
*data is_log_line_1 type zlog_table.
*
*parameters entry type string.
*
*call function 'ZFM_TEST'
*  exporting
*    iv_log_entry = entry
*  changing
*    is_log_line  = is_log_line_1.
*
*write: 'new log entry:', is_log_line_1-entry.



*************************
* Call interface method *
*************************
*data(x) = new zcl_misc( ).
*x->zif_hello_world~hello_world( ).



*****************
* select with ~ *
*****************
*select *
*       from spfli
*       into table @data(spfli_tab)
*       where spfli~carrid <> 'LH'.
*cl_demo_output=>display( spfli_tab ).



********************
* select-options   *
* selection-screen *
********************
*data lo_sflight type sflight.
*
*selection-screen begin of block block with frame title text-001.
*select-options criteria for lo_sflight-carrid.
*selection-screen end of block block.
*
*selection-screen begin of block block_1 with frame title text-002.
*parameters p type i.
*selection-screen end of block block_1.




********************
** Authority Check *
********************
*data x type sy-uname value 'DEVELOPER'.
*
*authority-check object 'S_CARRID' for user x
* id 'CARRID' field 'LH'
* id 'ACTVT' field '03'.
*if sy-subrc <> 0.
*  message 'not ok' type 'I'.
*else.
*  message 'ok' type 'I'.
*endif.



******************
* Event handling *
******************
*data(lo_handling) = new zcl_event_handling_class( ).
*data(lo_raising) = new zcl_event_raising_class( ).
*lo_raising->raise_event( ).



***********************************************
* Parameters with search help and value check *
***********************************************
  " search help
*parameters carrier type spfli-carrid default 'AA' matchcode object zhelp.
  " Only values from spfli allowed.
  " This is done by check table SCARR which is specified for field CARRID in dictionary.
*parameters carrier1 type spfli-carrid default 'AA' value check.
*write: carrier, / carrier1.



***************
* Potenzieren *
***************
*data(x) = ipow( base = 2 exp = 3 ).
*cl_demo_output=>display( x ).



**********************
* (move-)corresponding *
**********************
*data: begin of struct,
*        id  type i,
*        age type i,
*      end of struct.
*select single * from zmytable into corresponding fields of struct.
**select single * from zmytable into @data(lt_x).
**struct = corresponding #( lt_x ).
*cl_demo_output=>display( struct ).



*****************************************
* Insert and select from database table *
*****************************************
*data itab type standard table of zmytable.
*itab-id = 6. itab-name = 'Andy'. itab-age = 30. itab-profession = 'Asshole'.
*insert into zmytable values @itab.
*insert into zmytable values @( value #(
*  id = 6
*  name = 'Andy'
*  age = 30
*  profession = 'Asshole'
*) ).
*select * from zmytable into table itab.
*cl_demo_output=>display( itab ).



*************
* Templates *
*************
*data test(10) type c VALUE 'xxx'.
**WRITE |Variable Test hat Wert:  { test } |.
*cl_demo_output=>display( |Variable Test hat Wert:\n{ test }| ). "\n funktioniert nicht mit WRITE



*****************
* Textbausteine *
*****************
*WRITE 'Textbaustein 1'(001).
*WRITE TEXT-002.
