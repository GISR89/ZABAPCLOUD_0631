CLASS zcl_work_or_crud_test_0631 DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS :
      test_create_work_order EXPORTING et_ztwork_order_631_error TYPE ztt_work_order_631_2,

      test_read_work_order EXPORTING et_read_work_order TYPE ztt_work_order_631_2,

      test_update_work_order  EXPORTING et_ztwork_order_631_update TYPE ztt_work_order_631_2,

      test_delete_work_order EXPORTING et_ztwork_order_631_delete TYPE ztt_work_order_631_2.


    INTERFACES if_oo_adt_classrun.
PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_work_or_crud_test_0631 IMPLEMENTATION.


  METHOD if_oo_adt_classrun~main.

*    me->test_create_work_order( IMPORTING et_ztwork_order_631_error = DATA(lt_ztwork_order_631_error)  ).
*    out->write( data = lt_ztwork_order_631_error
*                name = 'Registros No validos' ).
*
*    me->test_read_work_order( IMPORTING et_read_work_order = DATA(lt_read_work_order) ).
*    out->write( lt_read_work_order ).
*
*     me->test_update_work_order( IMPORTING et_ztwork_order_631_update = DATA(lt_ztwork_order_update_error) ).
*     out->write( data = lt_ztwork_order_update_error
*                 name = 'Registros con errores en update' ).
*
     me->test_delete_work_order( importing et_ztwork_order_631_delete = data(lt_ztwork_order_delete) ).
     out->write( data = lt_ztwork_order_delete
                 name = 'Registros con errores en delete' ).


  ENDMETHOD.


  METHOD test_create_work_order.

    DATA(lo_work_order) = NEW zcl_work_or_crud_handler_0631( ).
*         DELETE FROM ztwork_order_631.
*         DELETE FROM ztwork_or_hist31.

    DATA(lt_ztwork_order_631) = VALUE ztt_work_order_631( ( customer_id   = '0010'
                                                            technician_id = '1234'
                                                            creation_date = '20250211'
                                                            priority      = 'A'
                                                            description   = 'Actualizar programa' )

                                                          ( customer_id   = '0020'
                                                            technician_id = '1250'
                                                            creation_date = '20250212'
                                                            priority      = 'B'
                                                            description   = 'Actualizar programa' )

                                                          ( customer_id   = '0030'
                                                            technician_id = '1434'
                                                            creation_date = '20250216'
                                                            priority      = 'A'
                                                            description   = 'Actualizar programa' ) ).

    lo_work_order->create_work_order( EXPORTING it_ztwork_order_631 = lt_ztwork_order_631
                                      IMPORTING et_ztwork_order_631_error = DATA(lt_ztwork_order_631_error) ).

    et_ztwork_order_631_error = lt_ztwork_order_631_error.

  ENDMETHOD.


  METHOD test_read_work_order.

    DATA(lo_read_work_order) = NEW zcl_work_or_crud_handler_0631( ).

    lo_read_work_order->read_work_order( EXPORTING iv_work_order    = '0000000000'
                                                   iv_customer_id   = ' '
                                                   iv_status        = 'PE'
                                                   iv_creation_date = '00000000'
                                        IMPORTING  et_read_work_order = DATA(lt_read_work_order) ).

    et_read_work_order = lt_read_work_order.

  ENDMETHOD.


  METHOD test_update_work_order.

    DATA(lo_test_update_work_order) = NEW zcl_work_or_crud_handler_0631( ).

    DATA(lt_ztwork_order_631_update) = VALUE ztt_work_order_631( ( work_order_id = '0000000007'
                                                                   status        = 'CO'
                                                                   priority      = ' ' ) ).

    lo_test_update_work_order->update_work_order( EXPORTING it_ztwork_order_631_update = lt_ztwork_order_631_update
                                                  IMPORTING et_ztwork_order_631_update = DATA(lt_ztwork_order_update_error) ).

    et_ztwork_order_631_update = lt_ztwork_order_update_error.

  ENDMETHOD.

  METHOD test_delete_work_order.

    DATA(lo_test_test_delete_work_order) = NEW zcl_work_or_crud_handler_0631( ).

    DATA(lt_ztwork_order_631_delete) = VALUE ztt_work_order_631( ( work_order_id = '0000000004' ) ).


    lo_test_test_delete_work_order->delete_work_order( EXPORTING it_ztwork_order_631_delete = lt_ztwork_order_631_delete
                                                       IMPORTING et_ztwork_order_631_delete = DATA(lt_ztwork_order_delete) ).

    et_ztwork_order_631_delete = lt_ztwork_order_delete.


  ENDMETHOD.

ENDCLASS.
