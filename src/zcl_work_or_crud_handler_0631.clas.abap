CLASS zcl_work_or_crud_handler_0631 DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS:
      create_work_order IMPORTING it_ztwork_order_631       TYPE ztt_work_order_631
                        EXPORTING et_ztwork_order_631_error TYPE ztt_work_order_631,
      read_work_order   IMPORTING iv_work_order      TYPE zde_work_order_id_0631 OPTIONAL
                                  iv_customer_id     TYPE zde_customer_id_0631 OPTIONAL
                                  iv_status          TYPE zde_status_0631 OPTIONAL
                                  iv_creation_date   TYPE zde_creation_date_0631 OPTIONAL
                        EXPORTING et_read_work_order TYPE ztt_work_order_631,
      update_work_order IMPORTING it_ztwork_order_631_update TYPE ztt_work_order_631
                        EXPORTING et_ztwork_order_631_update TYPE ztt_work_order_631.

  PROTECTED SECTION.
  PRIVATE SECTION.

ENDCLASS.



CLASS ZCL_WORK_OR_CRUD_HANDLER_0631 IMPLEMENTATION.


  METHOD create_work_order.

    DATA(lo_validator) = NEW zcl_work_order_validator_0631(  ).

    DATA lv_index TYPE i VALUE 1.

    SELECT FROM ztwork_order_631
         FIELDS MAX( work_order_id )
         INTO @DATA(lv_last_work_order_id).

    DATA(lt_ztwork_order_631_aux) = it_ztwork_order_631.
    DATA lt_ztwork_order_631_error TYPE ztt_work_order_631.

    LOOP AT lt_ztwork_order_631_aux ASSIGNING FIELD-SYMBOL(<fs_ztwork_order_631_aux>).
      DATA(lv_tabix_aux) = sy-tabix.

      <fs_ztwork_order_631_aux>-client = sy-mandt.
      <fs_ztwork_order_631_aux>-status = 'PE'.

      IF    lo_validator->validate_create_order( iv_customer_id   = <fs_ztwork_order_631_aux>-customer_id
                                                 iv_technician_id = <fs_ztwork_order_631_aux>-technician_id
                                                 iv_priority      = <fs_ztwork_order_631_aux>-priority ) = abap_true.
        IF lv_index = 1.
          <fs_ztwork_order_631_aux>-work_order_id = lv_last_work_order_id + 1.
          DATA(lv_current_work_order_id) = <fs_ztwork_order_631_aux>-work_order_id.
        ELSE.
          lv_current_work_order_id = lv_current_work_order_id + 1.
          <fs_ztwork_order_631_aux>-work_order_id = lv_current_work_order_id.
        ENDIF.

        lv_index = lv_index + 1.

      ELSE.

        APPEND <fs_ztwork_order_631_aux> TO lt_ztwork_order_631_error.
        DELETE lt_ztwork_order_631_aux INDEX lv_tabix_aux.

      ENDIF.
    ENDLOOP.

    IF lt_ztwork_order_631_aux IS NOT INITIAL.
      INSERT ztwork_order_631 FROM TABLE @lt_ztwork_order_631_aux.
    ENDIF.

    et_ztwork_order_631_error = lt_ztwork_order_631_error .

  ENDMETHOD.


  METHOD read_work_order.

    DATA lv_where_conditions TYPE string.
    DATA lx_dynamic_osql TYPE REF TO cx_root.

    IF iv_work_order IS NOT INITIAL
      AND iv_work_order <> '0000000000'.
      lv_where_conditions = |work_order_id eq '{ iv_work_order }'|.
    ENDIF.

    IF iv_customer_id IS NOT INITIAL.
      IF lv_where_conditions IS INITIAL.
        lv_where_conditions = |customer_id eq '{ iv_customer_id }'|.
      ELSE.
        lv_where_conditions = |{ lv_where_conditions }and customer_id eq '{ iv_customer_id }'|.
      ENDIF.
    ENDIF.

    IF iv_status IS NOT INITIAL.
      IF lv_where_conditions IS INITIAL.
        lv_where_conditions = |status eq '{ iv_status }'|.
      ELSE.
        lv_where_conditions = |{ lv_where_conditions }and status eq '{ iv_status }'|.
      ENDIF.
    ENDIF.

    IF iv_creation_date IS NOT INITIAL.
      IF lv_where_conditions IS INITIAL.
        lv_where_conditions = |creation_date eq '{ iv_creation_date }'|.
      ELSE.
        lv_where_conditions = |{ lv_where_conditions }and creation_date eq '{ iv_creation_date }'|.
      ENDIF.
    ENDIF.

    TRY.

        SELECT FROM ztwork_order_631
               FIELDS *
               WHERE (lv_where_conditions)
        INTO TABLE @DATA(lt_read_work_order).

      CATCH cx_sy_dynamic_osql_syntax
            cx_sy_dynamic_osql_semantics
            cx_sy_dynamic_osql_error INTO lx_dynamic_osql.

        RETURN.
    ENDTRY.

    et_read_work_order = lt_read_work_order.

  ENDMETHOD.


  METHOD update_work_order.

    DATA(lt_ztwork_order_update_aux) = it_ztwork_order_631_update.

    LOOP AT lt_ztwork_order_update_aux ASSIGNING FIELD-SYMBOL(<fs_ztwork_order_update_aux>).

      TRY.
          DATA(lo_lock_object) = cl_abap_lock_object_factory=>get_instance( EXPORTING iv_name = 'EZ_WORK_ORDER' ).

        CATCH cx_abap_lock_failure.
      ENDTRY.

      DATA lt_parameter TYPE if_abap_lock_object=>tt_parameter.

      lt_parameter = VALUE #( ( name = 'work_order_id'
                                value = REF #( <fs_ztwork_order_update_aux>-work_order_id ) ) ).

      TRY.
          lo_lock_object->enqueue( it_parameter  = lt_parameter ).

          DATA(lo_validator_update) = NEW zcl_work_order_validator_0631( ).

          SELECT SINGLE FROM ztwork_order_631
                   FIELDS *
                    WHERE work_order_id EQ @<fs_ztwork_order_update_aux>-work_order_id
                    INTO @DATA(ls_zswork_order_631).

          IF lo_validator_update->validate_update_order( iv_status_original = ls_zswork_order_631-status
                                                         iv_status          = <fs_ztwork_order_update_aux>-status
                                                         iv_priority        = <fs_ztwork_order_update_aux>-priority ).

            IF <fs_ztwork_order_update_aux>-status IS NOT INITIAL
            AND <fs_ztwork_order_update_aux>-status <> ' '.
              ls_zswork_order_631-status = <fs_ztwork_order_update_aux>-status.
            ENDIF.

            IF <fs_ztwork_order_update_aux>-priority IS NOT INITIAL
           AND <fs_ztwork_order_update_aux>-priority <> ' '.
              ls_zswork_order_631-priority = <fs_ztwork_order_update_aux>-priority.
            ENDIF.

            UPDATE ztwork_order_631 FROM @ls_zswork_order_631.
            IF sy-subrc NE 0.
              APPEND <fs_ztwork_order_update_aux> TO et_ztwork_order_631_update.
            ENDIF.
          ELSE.
            APPEND <fs_ztwork_order_update_aux> TO et_ztwork_order_631_update.
          ENDIF.

        CATCH cx_abap_foreign_lock cx_abap_lock_failure.
      ENDTRY.

      TRY.
          lo_lock_object->dequeue( it_parameter  = lt_parameter ).

        CATCH cx_abap_lock_failure.
      ENDTRY.
    ENDLOOP.


  ENDMETHOD.
ENDCLASS.
