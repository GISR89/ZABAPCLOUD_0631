CLASS zcl_work_order_validator_0631 DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS:
      validate_create_order IMPORTING iv_customer_id   TYPE zde_customer_id_0631
                                      iv_technician_id TYPE zde_technician_id_0631
                                      iv_priority      TYPE zde_priority_0631
                            RETURNING VALUE(rv_valid)  TYPE abap_bool,

      validate_update_order IMPORTING iv_status          TYPE zde_status_0631
                                      iv_priority        TYPE zde_priority_0631
                                      iv_status_original TYPE zde_status_0631
                            RETURNING VALUE(rv_valid)    TYPE abap_bool,

      validate_status_and_priority IMPORTING iv_status       TYPE zde_status_0631
                                             iv_priority     TYPE zde_priority_0631
                                   RETURNING VALUE(rv_valid) TYPE abap_bool,

      validate_delete_order IMPORTING iv_work_order_id TYPE zde_work_order_id_0631
                                      iv_status        TYPE zde_status_0631
                            RETURNING VALUE(rv_valid)  TYPE abap_bool,

      validate_authority    IMPORTING iv_work_order_id TYPE zde_work_order_id_0631
                                      iv_actvt         TYPE c
                            RETURNING VALUE(rv_valid) TYPE abap_bool.

PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_WORK_ORDER_VALIDATOR_0631 IMPLEMENTATION.


  METHOD validate_create_order.

    rv_valid = abap_true.

    SELECT SINGLE FROM ztcustomer_0631
             FIELDS @abap_true
             WHERE customer_id = @iv_customer_id
        INTO @DATA(lv_customer_valid).

    IF lv_customer_valid <> abap_true.
      rv_valid = abap_false.
      EXIT.
    ENDIF.

    SELECT SINGLE FROM zttechnician_631
            FIELDS @abap_true
            WHERE technician_id = @iv_technician_id
        INTO @DATA(lv_technician_valid).

    IF lv_technician_valid <> abap_true.
      rv_valid = abap_false.
      EXIT.
    ENDIF.

    IF NOT me->validate_status_and_priority( iv_status = ''
                                     iv_priority = iv_priority ).
      rv_valid = abap_false.
      EXIT.
    ENDIF.

  ENDMETHOD.

  METHOD validate_status_and_priority.

    rv_valid = abap_true.

     IF iv_status IS NOT INITIAL
    AND iv_status <> ' '.
      SELECT SINGLE FROM ztstatus_0631
              FIELDS @abap_true
              WHERE status_code = @iv_status
         INTO @DATA(lv_status_valid).

      IF lv_status_valid <> abap_true.
        rv_valid = abap_false.
        EXIT.
      ENDIF.
    ENDIF.

    IF iv_priority IS NOT INITIAL
    AND iv_priority <> ' '.
    SELECT SINGLE FROM ztpriority_0631
            FIELDS @abap_true
            WHERE priority_code = @iv_priority
       INTO @DATA(lv_priority_valid).

    IF lv_priority_valid <> abap_true.
      rv_valid = abap_false.
      EXIT.
    ENDIF.
    endif.

  ENDMETHOD.

  METHOD validate_update_order.

    rv_valid = abap_true.

    IF iv_status_original <> 'PE'.
      rv_valid = abap_false.
      EXIT.
    ENDIF.

    IF NOT me->validate_status_and_priority( iv_status   = iv_status
                                             iv_priority = iv_priority ).
      rv_valid = abap_false.
      EXIT.
    ENDIF.
  ENDMETHOD.

    METHOD validate_delete_order.
      rv_valid = abap_true.

      SELECT SINGLE FROM ztwork_or_hist31
             FIELDS @abap_true
             WHERE work_order_id = @iv_work_order_id AND
                   change_description NE 'ACTUALIZADO'
            INTO @DATA(lv_work_order_valid).


      IF iv_status <> 'PE' AND lv_work_order_valid <> abap_true.
        rv_valid = abap_false.
        EXIT.
      ENDIF.

    ENDMETHOD.

  METHOD validate_authority.

    AUTHORITY-CHECK OBJECT 'ZAO_WORKOR'
     ID 'ZAF_WORKOR' FIELD iv_work_order_id
     ID 'ACTVT' FIELD iv_actvt  .

    IF sy-subrc EQ 0.
      rv_valid = abap_true.
      exit.
    ELSE.
      rv_valid = abap_false.
      EXIT.
    ENDIF.


  ENDMETHOD.

ENDCLASS.
