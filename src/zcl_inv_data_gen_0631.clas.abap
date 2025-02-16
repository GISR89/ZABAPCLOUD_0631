CLASS zcl_inv_data_gen_0631 DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

  METHODS generation_data.
INTERFACES if_oo_adt_classrun.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_INV_DATA_GEN_0631 IMPLEMENTATION.


  METHOD generation_data.


  ENDMETHOD.


  METHOD if_oo_adt_classrun~main.

    "for ztstatus_0631
    DATA(lt_ztstatus_0631) = VALUE ztt_status_0631( ( status_code = 'PE'
                                                      status_description = 'Pending' )
                                                    ( status_code = 'CO'
                                                      status_description = 'Completed' ) ).
    INSERT ztstatus_0631 FROM TABLE @lt_ztstatus_0631.

    "for ztpriority_0631
    DATA(lt_ztpriority_0631) = VALUE ztt_priority_0631( ( priority_code        = 'A'
                                                          priority_description = 'High' )
                                                        ( priority_code        = 'B'
                                                          priority_description = 'Low' ) ).
    INSERT ztpriority_0631 FROM TABLE @lt_ztpriority_0631.

    "for ztcustomer_0631
    DATA(lt_ztcustomer_0631) = VALUE zttcustomer_0631( ( client      = sy-mandt
                                                         customer_id = '0010'
                                                         name        = 'Logali sa'
                                                         address     = 'calle falsa 1236'
                                                         phone       = '+541101234567' )

                                                       ( client      = sy-mandt
                                                         customer_id = '0020'
                                                         name        = 'Pepsi sa'
                                                         address     = 'calle desconocido 2040'
                                                         phone       = '+541101234348' )

                                                       ( client      = sy-mandt
                                                         customer_id = '0030'
                                                         name        = 'Coca-Cola sa'
                                                         address     = 'calle desconocido 3040'
                                                         phone       = '+541101234000' ) ).
    INSERT ztcustomer_0631 FROM TABLE @lt_ztcustomer_0631.

    " zttechnician_631
    DATA(lt_zttechnician_631) = VALUE ztt_technician_631( ( technician_id = '1234'
                                                            name_tec      = 'Pepito Perez'
                                                            specialty     = 'Desarrollo' )

                                                          ( technician_id = '1250'
                                                            name_tec      = 'Luis Rodriguez'
                                                            specialty     = 'Mantenimiento' )

                                                          ( technician_id = '1434'
                                                            name_tec      = 'Roberto Lopez'
                                                            specialty     = 'Optimizacion'  ) ).
    INSERT zttechnician_631 FROM TABLE @lt_zttechnician_631.


    "for ztwork_or_hist31
*    MODIFY ztwork_or_hist31 FROM TABLE @( VALUE #( ( history_id         = '000100'
*                                                     work_order_id      = '0001'
*                                                     modification_date  = '20250130'
*                                                     change_description = 'Crear tabla' )
*
*                                                   ( history_id         = '000200'
*                                                     work_order_id      = '0002'
*                                                     modification_date  = '20250131'
*                                                     change_description = 'Actualizar regristro'  )
*
*                                                   ( history_id         = '000300'
*                                                     work_order_id      = '0003'
*                                                     modification_date  = '20250120'
*                                                     change_description = 'Crear tabla'  )
*
*                                                   ( history_id         = '000400'
*                                                     work_order_id      = '0004'
*                                                     modification_date  = '20250202'
*                                                     change_description = 'Eliminar regristro' ) ) ).

  ENDMETHOD.
ENDCLASS.
