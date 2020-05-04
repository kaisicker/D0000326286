*&---------------------------------------------------------------------*
*& Report  ZKS_CSV_UPLOAD_AND_INSERT
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
report zks_csv_upload_and_insert.

"-Begin classes -----------------------------------------------------------------

class z_csv_import definition.

  public section.

    methods constructor.

    methods load_csv_file
      importing
        value(i_filename) type string.

    methods transform_csv_data
      importing
        value(i_tablename) type string.

    methods get_csv_data
      importing
        value(i_tablename) type string.

  protected section.

    data      g_csv_data type stringtab.
    data      g_error_flag type abap_bool value abap_false.
    data      g_tab_fields type standard table of string.
    constants c_tab type c value cl_abap_char_utilities=>horizontal_tab. " Tab

  private section.

endclass.



class z_csv_import implementation.


  method constructor.
    "-Begin-----------------------------------------------------------------

    clear g_csv_data.

    "-End-------------------------------------------------------------------
  endmethod.


  method get_csv_data.
    "-Begin-----------------------------------------------------------------

    "-Structures----------------------------------------------------------
    types: begin of l_typ_confrontation,
             intfieldname type string,
             intfieldpos  type i,
             intfieldtyp  type string,
             csvfieldpos  type i,
             csvfieldname type string,
           end of l_typ_confrontation.

    "-Variables-----------------------------------------------------------
    data l_rda_data type ref to data.
    data l_rda_wa type ref to data.
    data l_rcl_descr_tab type ref to cl_abap_tabledescr.
    data l_rcl_descr_struc type ref to cl_abap_structdescr.
    data l_comp_descr type abap_compdescr.
    data l_tab_content type standard table of string.
    data l_line type string value ''.
    data l_tab_confrontation type standard table of l_typ_confrontation
      with key csvfieldpos.
    data l_fieldname type string value ''.
    data l_content type string value ''.
    data l_conf type l_typ_confrontation.

    field-symbols  <l_table> type standard table.
    field-symbols  <l_comp> type any.
    field-symbols  <l_wa> type any.

    "-Main----------------------------------------------------------------
    if g_csv_data is not initial and g_error_flag = abap_false.

      "-Reference to Table----------------------------------------------
      create data l_rda_data type standard table of (i_tablename).
      assign l_rda_data->* to  <l_table>.
      "-Get Structure of Table------------------------------------------
      l_rcl_descr_tab ?= cl_abap_typedescr=>describe_by_data(  <l_table> ).
      l_rcl_descr_struc ?= l_rcl_descr_tab->get_table_line_type( ).
      "-Define Line of Table--------------------------------------------
      create data l_rda_wa like line of  <l_table>.
      assign l_rda_wa->* to  <l_wa>.

      "-Compare Field Names of the Table with Headline of CSV-----------
      "-
      "- With this step is the position of the column indifferent. It
      "- is only necessary that the field of the table and the column
      "- of the CSV file must have the same name.
      "-
      "-----------------------------------------------------------------
      loop at l_rcl_descr_struc->components into l_comp_descr.
        l_conf-intfieldname = l_comp_descr-name.
        l_conf-intfieldpos = sy-tabix.
        l_conf-intfieldtyp = l_comp_descr-type_kind.
        loop at g_tab_fields into l_fieldname.
          l_conf-csvfieldpos = -1.
          l_conf-csvfieldname = 'UNKNOWN'.
          if l_comp_descr-name = l_fieldname.
            l_conf-csvfieldname = l_fieldname.
            l_conf-csvfieldpos = sy-tabix.
            exit.
          endif.
        endloop.
        append l_conf to l_tab_confrontation.
      endloop.
      delete l_tab_confrontation where csvfieldpos = -1.
      sort l_tab_confrontation by csvfieldpos.

      "-Copy Data-------------------------------------------------------
      loop at g_csv_data into l_line from 7.
*        split l_line at ';' into table l_tab_content.
        split l_line at c_tab into table l_tab_content.
        loop at l_tab_content into l_content.
          condense l_content.
          read table l_tab_confrontation with key csvfieldpos = sy-tabix
            into l_conf.
          if sy-subrc = 0.
            assign component l_conf-intfieldname of structure  <l_wa>
              to  <l_comp>.
            if l_conf-intfieldtyp = 'P'.
              replace all occurrences of '.' in l_content with ''.
              replace ',' in l_content with '.'.
              <l_comp> = l_content.
            else.
              <l_comp> = l_content.
            endif.
          endif.
        endloop.
        append  <l_wa> to  <l_table>.
        clear  <l_wa>.
      endloop.

      "-Write Data into Table-------------------------------------------
      insert (i_tablename) from table  <l_table>.
*      update (i_tablename) from <l_line>.
*      loop at <l_table> assigning field-symbol(<l_line>).
*        insert (i_tablename) from <l_line>.
      if sy-subrc  <> 0.
        g_error_flag = abap_true.
      endif.
*    endloop.

    endif.

    "-End-------------------------------------------------------------------
  endmethod.


  method load_csv_file.
    "-Begin-----------------------------------------------------------------

    call function 'GUI_UPLOAD'
      exporting
        filename = i_filename
        filetype = 'ASC'
      tables
        data_tab = g_csv_data
      exceptions
        others   = 1.
    if sy-subrc  <> 0.
      g_error_flag = abap_true.
    endif.

    "-End-------------------------------------------------------------------
  endmethod.


  method transform_csv_data.
    "-Begin-----------------------------------------------------------------

    "-Variables-----------------------------------------------------------
    data l_fld1 type string value ''.
    data l_fld2 type string value ''.
    data l_fld3 type string value ''.
    data l_fldrest type string value ''.

    field-symbols  <line> type string.

    "-Main----------------------------------------------------------------
    if g_csv_data is not initial and g_error_flag = abap_false.

      "-Manipulate Headline---------------------------------------------
      read table g_csv_data index 4 assigning  <line>.
*      <line> = 'MANDT;' &&  <line>.
      <line> = 'MANDT' &&  <line>.

      condense  <line> no-gaps.
*      split  <line> at ';' into table g_tab_fields.
      split <line> at c_tab into table g_tab_fields.

    endif.

    "-Transformation----------------------------------------------------
    loop at g_csv_data from 2 assigning  <line>.
*      <line> = sy-mandt && ';' &&  <line>.
      <line> = sy-mandt &&  <line>.
    endloop.

    "-End-------------------------------------------------------------------
  endmethod.

endclass.

"-End classes -------------------------------------------------------------------






start-of-selection.

  "-Variables-----------------------------------------------------------
  data csv type ref to z_csv_import.

  "-Main----------------------------------------------------------------
  delete from zmi_ps_vn.
  commit work.

  create object csv.
  csv->load_csv_file( 'D:\Users\BKU\kaisicker\Kai Sicker\zmi_ps_vn_100000.txt' ).
  csv->transform_csv_data( 'ZMI_PS_VN' ).
  csv->get_csv_data( 'ZMI_PS_VN' ).
