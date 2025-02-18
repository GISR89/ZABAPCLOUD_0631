CLASS zcl_work_or_crud_handler_0631 DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS:
      create_work_order IMPORTING it_ztwork_order_631       TYPE ztt_work_order_631
                        EXPORTING et_ztwork_order_631_error TYPE ztt_work_order_631_2,

      read_work_order   IMPORTING iv_work_order      TYPE zde_work_order_id_0631 OPTIONAL
                                  iv_customer_id     TYPE zde_customer_id_0631 OPTIONAL
                                  iv_status          TYPE zde_status_0631 OPTIONAL
                                  iv_creation_date   TYPE zde_creation_date_0631 OPTIONAL
                        EXPORTING et_read_work_order TYPE ztt_work_order_631_2,

      update_work_order IMPORTING it_ztwork_order_631_update TYPE ztt_work_order_631
                        EXPORTING et_ztwork_order_631_update TYPE ztt_work_order_631_2,

      delete_work_order IMPORTING it_ztwork_order_631_delete TYPE ztt_work_order_631
                        EXPORTING et_ztwork_order_631_delete TYPE ztt_work_order_631_2,

      create_work_order_hist IMPORTING iv_work_order  TYPE zde_work_order_id_0631
                                       iv_change_desc TYPE zde_change_desc_0631.

    DATA ls_zswork_order_631_2 TYPE zswork_order_631_2.

  PROTECTED SECTION.
  PRIVATE SECTION.

ENDCLASS.

CLASS zcl_work_or_crud_handler_0631 IMPLEMENTATION.

  METHOD create_work_order.
"se declaran objetos e instancia
    DATA(lo_validator) = NEW zcl_work_order_validator_0631(  ).
    DATA(lt_ztwork_order_631_aux) = it_ztwork_order_631.
"se realiza un recorrido de los datos recibidos y se asigna un field-symbol para poder trabajar con los campos.
    LOOP AT lt_ztwork_order_631_aux ASSIGNING FIELD-SYMBOL(<fs_ztwork_order_631_aux>) .
"se verifica el authority-check
      IF   lo_validator->validate_authority(  iv_work_order_id = <fs_ztwork_order_631_aux>-work_order_id
                                            iv_actvt = '01' ) = abap_false.
        ls_zswork_order_631_2-message = 'No esta autorizado para crear'.
        APPEND ls_zswork_order_631_2 TO et_ztwork_order_631_error.
        EXIT.
      ENDIF.
"se realiza una lectura de la tabla para obtener el ultimo work_order_id. Es para que los que se creen tengan un id consecutivo.
      SELECT FROM ztwork_order_631
           FIELDS MAX( work_order_id )
           INTO @DATA(lv_last_work_order_id).

      <fs_ztwork_order_631_aux>-client = sy-mandt.
      <fs_ztwork_order_631_aux>-status = 'PE'.
      <fs_ztwork_order_631_aux>-work_order_id = lv_last_work_order_id + 1.

"se realiza la validacion si existe customer_id, technician_id y priority
      IF    lo_validator->validate_create_order( iv_customer_id   = <fs_ztwork_order_631_aux>-customer_id
                                                 iv_technician_id = <fs_ztwork_order_631_aux>-technician_id
                                                 iv_priority      = <fs_ztwork_order_631_aux>-priority ) = abap_false.
        MOVE-CORRESPONDING <fs_ztwork_order_631_aux> TO ls_zswork_order_631_2.
        ls_zswork_order_631_2-message = 'Customer_id, technician_id o priority no valido'.
        APPEND ls_zswork_order_631_2 TO et_ztwork_order_631_error.
        CONTINUE.
      ENDIF.
"se insertan los datos en la tabla de base de datos
      INSERT ztwork_order_631 FROM @<fs_ztwork_order_631_aux>.
      IF sy-subrc EQ 0.
"se crea el registro para la tabla ztwork_or_hist31 (tabla de historico)
        me->create_work_order_hist( iv_work_order = <fs_ztwork_order_631_aux>-work_order_id
                                        iv_change_desc = 'CREADO' ).
      ELSE.
        MOVE-CORRESPONDING <fs_ztwork_order_631_aux> TO ls_zswork_order_631_2.
        ls_zswork_order_631_2-message = 'El registro no fue creado'.
        APPEND ls_zswork_order_631_2 TO et_ztwork_order_631_error.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.

  METHOD read_work_order.
"se declaran objetos e instancias
    DATA lv_where_conditions TYPE string.
    DATA lx_dynamic_osql TYPE REF TO cx_root.
    DATA(lo_validator) = NEW zcl_work_order_validator_0631(  ).
" validacion de authority-check
    IF     lo_validator->validate_authority( iv_work_order_id = iv_work_order
                                         iv_actvt = '03' ) = abap_false.
      ls_zswork_order_631_2-message = 'No esta autorizado para leer'.
      APPEND ls_zswork_order_631_2 TO et_read_work_order.
      EXIT.
    ENDIF.
"Se indican codiciones para el where
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
"Se realiza una lectura dinamica de la base de datos
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
"se declaran objetos e instancia
    DATA(lt_ztwork_order_update_aux) = it_ztwork_order_631_update.
    DATA(lo_validator_update) = NEW zcl_work_order_validator_0631( ).
"se realiza un recorrido de los datos recibidos y se asigna un field-symbol para poder trabajar con los campos.
    LOOP AT lt_ztwork_order_update_aux ASSIGNING FIELD-SYMBOL(<fs_ztwork_order_update_aux>).
"se verifica el authority-check
      IF  lo_validator_update->validate_authority( iv_work_order_id = <fs_ztwork_order_update_aux>-work_order_id
                                       iv_actvt = '02' ) = abap_false.

        MOVE-CORRESPONDING <fs_ztwork_order_update_aux> TO ls_zswork_order_631_2.
        ls_zswork_order_631_2-message = 'No esta autorizado para modificar'.
        APPEND ls_zswork_order_631_2 TO et_ztwork_order_631_update.
        CONTINUE.
      ENDIF.
"Se bloquea el objeto
      TRY.
          DATA(lo_lock_object) = cl_abap_lock_object_factory=>get_instance( EXPORTING iv_name = 'EZ_WORK_ORDER' ).

        CATCH cx_abap_lock_failure.
      ENDTRY.

      DATA lt_parameter TYPE if_abap_lock_object=>tt_parameter.

      lt_parameter = VALUE #( ( name = 'work_order_id'
                                value = REF #( <fs_ztwork_order_update_aux>-work_order_id ) ) ).

      TRY.
          lo_lock_object->enqueue( it_parameter  = lt_parameter ).
"Se realiza una lectura de la tabla para traer los registros que se van a modificar
          SELECT SINGLE FROM ztwork_order_631
                   FIELDS *
                    WHERE work_order_id EQ @<fs_ztwork_order_update_aux>-work_order_id
                    INTO @DATA(ls_zswork_order_631).
"Se verifica que los registros tengan un status y prioridad valido
          IF lo_validator_update->validate_update_order( iv_status_original = ls_zswork_order_631-status
                                                         iv_status          = <fs_ztwork_order_update_aux>-status
                                                         iv_priority        = <fs_ztwork_order_update_aux>-priority ) = abap_false.

            MOVE-CORRESPONDING <fs_ztwork_order_update_aux> TO ls_zswork_order_631_2.
            ls_zswork_order_631_2-message = 'Status o Priority no validos'.
            APPEND ls_zswork_order_631_2 TO et_ztwork_order_631_update.
            CONTINUE.
          ENDIF.
"teniendo en cuenta que se puede modificar tanto el estado como la prioridad, se colocan las siguientes condiciones.
          IF <fs_ztwork_order_update_aux>-status IS NOT INITIAL
          AND <fs_ztwork_order_update_aux>-status <> ' '.
            ls_zswork_order_631-status = <fs_ztwork_order_update_aux>-status.
          ENDIF.

          IF <fs_ztwork_order_update_aux>-priority IS NOT INITIAL
         AND <fs_ztwork_order_update_aux>-priority <> ' '.
            ls_zswork_order_631-priority = <fs_ztwork_order_update_aux>-priority.
          ENDIF.
"Se realiza el update
          UPDATE ztwork_order_631 FROM @ls_zswork_order_631.
"Si hubo algun error arroja el siguiente mensaje en el campo message.
          IF sy-subrc NE 0.
            MOVE-CORRESPONDING <fs_ztwork_order_update_aux> TO ls_zswork_order_631_2.
            ls_zswork_order_631_2-message = 'El regitro no se ha modificado'.
            APPEND ls_zswork_order_631_2 TO et_ztwork_order_631_update.
            CONTINUE.

          ENDIF.
"si esta todo ok, crea el regristro de la modificacion en la tabla de historico.
          me->create_work_order_hist( iv_work_order = ls_zswork_order_631-work_order_id
                                      iv_change_desc = 'ACTUALIZADO' ).
"si el objeto esta bloqueado por otro usuario arrojara este mensaje.
        CATCH cx_abap_foreign_lock cx_abap_lock_failure.
          MOVE-CORRESPONDING <fs_ztwork_order_update_aux>  TO ls_zswork_order_631_2.
          ls_zswork_order_631_2-message = 'Work_order_id bloqueado'.
          APPEND ls_zswork_order_631_2 TO et_ztwork_order_631_update.
          CONTINUE.
      ENDTRY.
" se libera el objeto.
      TRY.
          lo_lock_object->dequeue( it_parameter  = lt_parameter ).

        CATCH cx_abap_lock_failure.
      ENDTRY.
    ENDLOOP.

  ENDMETHOD.

  METHOD delete_work_order.
"se declaran objetos e instancia
    DATA(lo_validator_delete) = NEW zcl_work_order_validator_0631( ).
    DATA(lt_ztwork_order631_aux) = it_ztwork_order_631_delete.
"se realiza un recorrido de los datos recibidos y se asigna un field-symbol para poder trabajar con los campos.
    LOOP AT lt_ztwork_order631_aux ASSIGNING FIELD-SYMBOL(<fs_ztwork_order631_aux>).
"se verifica el authority-check
      IF  lo_validator_delete->validate_authority( iv_work_order_id = <fs_ztwork_order631_aux>-work_order_id
                                                    iv_actvt = '06' ) = abap_false.

        MOVE-CORRESPONDING <fs_ztwork_order631_aux> TO ls_zswork_order_631_2.
        ls_zswork_order_631_2-message = 'No esta autorizado para borrar'.
        APPEND ls_zswork_order_631_2 TO et_ztwork_order_631_delete.
        CONTINUE.
      ENDIF.
"Se bloquea el objeto
      TRY.
          DATA(lo_lock_object) = cl_abap_lock_object_factory=>get_instance( EXPORTING iv_name = 'EZ_WORK_ORDER' ).

        CATCH cx_abap_lock_failure.
      ENDTRY.

      DATA lt_parameter TYPE if_abap_lock_object=>tt_parameter.

      lt_parameter = VALUE #( ( name = 'work_order_id'
                                value = REF #( <fs_ztwork_order631_aux>-work_order_id ) ) ).

      TRY.
          lo_lock_object->enqueue( it_parameter  = lt_parameter ).

          SELECT SINGLE FROM ztwork_order_631
            FIELDS *
            WHERE work_order_id EQ @<fs_ztwork_order631_aux>-work_order_id
            INTO @DATA(ls_ztwork_order_631).
"Se verifica que los registros tengan un status y prioridad valido
          IF lo_validator_delete->validate_delete_order( iv_work_order_id = ls_ztwork_order_631-work_order_id
                                                         iv_status = ls_ztwork_order_631-status ) = abap_false.
"en caso de que no pase la validacion se mostrara en consola el registro y el mensaje
            MOVE-CORRESPONDING <fs_ztwork_order631_aux> TO ls_zswork_order_631_2.
            ls_zswork_order_631_2-message = 'Status no valido'.
            APPEND ls_zswork_order_631_2 TO et_ztwork_order_631_delete.
            CONTINUE.
          ENDIF.
"Se borra el registro
          DELETE ztwork_order_631 FROM @ls_ztwork_order_631.
"En caso de error
          IF sy-subrc NE 0.
            MOVE-CORRESPONDING <fs_ztwork_order631_aux> TO ls_zswork_order_631_2.
            ls_zswork_order_631_2-message = 'El registro no se ha borrado'.
            APPEND ls_zswork_order_631_2 TO et_ztwork_order_631_delete.
            CONTINUE.

          ENDIF.
"Si todo esta ok, se crea el registro en la trabla de historico
          me->create_work_order_hist( iv_work_order = ls_ztwork_order_631-work_order_id
                                                    iv_change_desc = 'BORRADO' ).
"En caso de estar bloqueado por otro usuario, mostrara este mensaje sin realizar el delete.
        CATCH cx_abap_foreign_lock cx_abap_lock_failure.
          MOVE-CORRESPONDING <fs_ztwork_order631_aux> TO ls_zswork_order_631_2.
          ls_zswork_order_631_2-message = 'Work_order_id bloqueado'.
          APPEND ls_zswork_order_631_2 TO et_ztwork_order_631_delete.
          CONTINUE.
      ENDTRY.
"se libera el objeto
      TRY.
          lo_lock_object->dequeue( it_parameter  = lt_parameter ).

        CATCH cx_abap_lock_failure.
      ENDTRY.
    ENDLOOP.

  ENDMETHOD.

  METHOD create_work_order_hist.
"se realiza una lectura de la tabla para obtener el ultimo history_id. Es para que los que se creen tengan un id consecutivo.
    SELECT FROM ztwork_or_hist31
         FIELDS MAX( history_id )
         INTO @DATA(lv_last_history_id).
"Se declara una estructura en linea para asignar los valores a la tabla.
    DATA(ls_ztwork_or_hist31_aux) = VALUE zswork_or_hist31( client = sy-mandt
                                                            history_id = lv_last_history_id + 1
                                                            work_order_id = iv_work_order
                                                            modification_date = cl_abap_context_info=>get_system_date( )
                                                            change_description = iv_change_desc ).

    INSERT ztwork_or_hist31 FROM  @ls_ztwork_or_hist31_aux.


  ENDMETHOD.

ENDCLASS.
