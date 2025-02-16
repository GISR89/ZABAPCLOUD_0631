CLASS zcl_advqueries_0631 DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
  INTERFACES if_oo_adt_classrun.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_ADVQUERIES_0631 IMPLEMENTATION.


  METHOD if_oo_adt_classrun~main.

    TYPES : BEGIN OF ty_content,
              work_order_id      TYPE zde_work_order_id_0631,
              customer_id        TYPE zde_customer_id_0631,
              technician_id      TYPE zde_technician_id_0631,
              creation_date      TYPE zde_creation_date_0631,
              status             TYPE zde_status_0631,
              priority           TYPE zde_priority_0631,
              description        TYPE zde_description_0631,
              name               TYPE zde_name_0631,
              address            TYPE zde_address_0631,
              phone              TYPE zde_phone_0631,
              name_tec           TYPE zde_name_tec_0631,
              specialty          TYPE  zde_specialty_0631,
              history_id         TYPE zde_history_id_0631,
              modification_date  TYPE zde_modification_date_0631,
              change_description TYPE zde_change_desc_0631,
            END OF ty_content.

    DATA: lt_content          TYPE STANDARD TABLE OF ty_content,
          lv_datasource_name  TYPE string,
          lv_selected_column  TYPE string,
          lv_where_conditions TYPE string.

    DATA lx_dynamic_osql TYPE REF TO cx_root.

    lv_datasource_name = 'ztwork_order_631'. " ztwork_order_631 / ztcustomer_0631 / zttechnician_631 / ztwork_or_hist31

    IF lv_datasource_name EQ 'ztwork_order_631'.
      lv_selected_column = |work_order_id, customer_id, technician_id, creation_date, status, priority, description|.
      lv_where_conditions = |creation_date lt '20250201' and status eq 'PE'|.

    ELSEIF lv_datasource_name EQ 'ztcustomer_0631'.
      lv_selected_column = |customer_id, name, address, phone|.
      lv_where_conditions = |customer_id eq '0020'|.

    ELSEIF lv_datasource_name EQ 'zttechnician_631'.
      lv_selected_column = |technician_id, name_tec, specialty|.
      lv_where_conditions = |technician_id eq '1250|.

    ELSEIF lv_datasource_name EQ 'ztwork_or_hist31'.
      lv_selected_column = |history_id, work_order_id, modification_date, change_description|.
      lv_where_conditions = |history_id eq '0001'|.

    ELSE.
      RETURN.
    ENDIF.

    TRY.

        SELECT FROM (lv_datasource_name)
               FIELDS (lv_selected_column)
               WHERE (lv_where_conditions)
        INTO CORRESPONDING FIELDS OF TABLE @lt_content.


      CATCH cx_sy_dynamic_osql_syntax
            cx_sy_dynamic_osql_semantics
            cx_sy_dynamic_osql_error INTO lx_dynamic_osql.
        out->write( lx_dynamic_osql->get_text(  ) ).
        RETURN.
    ENDTRY.

    IF sy-subrc EQ 0.
      out->write( lt_content ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.
