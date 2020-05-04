class ZCL_ZKS_TEST_DD definition
  public
  inheriting from CL_SADL_GTK_EXPOSURE_MPC
  final
  create public .

public section.
protected section.

  methods GET_PATHS
    redefinition .
  methods GET_TIMESTAMP
    redefinition .
private section.
ENDCLASS.



CLASS ZCL_ZKS_TEST_DD IMPLEMENTATION.


  method GET_PATHS.
et_paths = VALUE #(
( `CDS~ZKS_TEST_DD` )
).
  endmethod.


  method GET_TIMESTAMP.
RV_TIMESTAMP = 20200428190314.
  endmethod.
ENDCLASS.
