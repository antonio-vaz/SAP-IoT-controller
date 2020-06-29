*&---------------------------------------------------------------------*
*& Report ZCL_IOT_CONTROLLER
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_iot_controller.

CLASS iot_controller DEFINITION.
  PUBLIC SECTION.
    TYPES:
      BEGIN OF ty_response, http_status_code TYPE i, http_status_text TYPE string, content TYPE string, END OF ty_response,
        BEGIN OF ty_rgb, red TYPE i, green TYPE i, blue TYPE i, END OF ty_rgb,
          BEGIN OF ty_rgbleb, brightness TYPE i, color TYPE ty_rgb, END OF ty_rgbleb,
            BEGIN OF ty_status, rgb_led TYPE ty_rgbleb, switch TYPE char10, END OF ty_status,
              BEGIN OF ty_switch, switch TYPE char10, END OF ty_switch,
                BEGIN OF ty_rgbpayload, brightness TYPE i, red TYPE i, green TYPE i, blue TYPE i, END OF ty_rgbpayload.

    METHODS: refresh_data, display, get_current_switch RETURNING VALUE(switch) TYPE i,
      turn_off_switch,
      turn_on_switch,
      trigger_rgb IMPORTING brightness TYPE i
                            red        TYPE i
                            green      TYPE i
                            blue       TYPE i.

  PRIVATE SECTION.
    DATA lt_data  TYPE TABLE OF ty_status.
    DATA alv TYPE REF TO cl_salv_table.

ENDCLASS.

CLASS lcl_handle_events DEFINITION.
  PUBLIC SECTION.
    METHODS:
      constructor IMPORTING iot_controller TYPE REF TO iot_controller,
      on_user_command FOR EVENT added_function OF cl_salv_events IMPORTING e_salv_function,
      handle_switch,
      handle_rgb.
  PRIVATE SECTION.
    DATA iot_controller TYPE REF TO iot_controller.
ENDCLASS.

CLASS lcl_handle_events IMPLEMENTATION.
  METHOD constructor.
    me->iot_controller = iot_controller.
  ENDMETHOD.

  METHOD on_user_command.
    CASE e_salv_function.
      WHEN '&SWT'.
        handle_switch( ).
      WHEN '&RGB'.
        handle_rgb( ).
    ENDCASE.
  ENDMETHOD.                    "on_user_command

  METHOD handle_switch.
    DATA(swt) = iot_controller->get_current_switch( ).
    IF swt = 1.
      iot_controller->turn_off_switch( ).
    ELSE.
      iot_controller->turn_on_switch( ).
    ENDIF.
  ENDMETHOD.

  METHOD handle_rgb.

    DATA: lt TYPE TABLE OF sval,
          lw LIKE LINE OF lt.

    lw-tabname = 'ZRGB'.
    lw-fieldname = 'BRIGHTNESS'.
    APPEND lw TO lt.
    lw-fieldname = 'RED'.
    APPEND lw TO lt.
    lw-fieldname = 'GREEN'.
    APPEND lw TO lt.
    lw-fieldname = 'BLUE'.
    APPEND lw TO lt.

    CALL FUNCTION 'POPUP_GET_VALUES'
      EXPORTING
        popup_title = 'RGB Led'
      TABLES
        fields      = lt.
    IF sy-subrc = 0.
      DATA: l_bright TYPE i, l_red TYPE i, l_green TYPE i, l_blue TYPE i.
      LOOP AT lt INTO lw.
        CASE lw-fieldname.
          WHEN 'BRIGHTNESS'.
            l_bright = lw-value.
          WHEN 'RED'.
            l_red = lw-value.
          WHEN 'GREEN'.
            l_green = lw-value.
          WHEN 'BLUE'.
            l_blue = lw-value.
        ENDCASE.

      ENDLOOP.

      iot_controller->trigger_rgb( brightness = l_bright
                                    red = l_red
                                    green = l_green
                                    blue = l_blue
       ).
    ENDIF.

  ENDMETHOD.
ENDCLASS.


CLASS iot_controller IMPLEMENTATION.
  METHOD trigger_rgb.
    DATA response TYPE ty_response.
    DATA payload TYPE ty_rgbpayload.
    payload-brightness = brightness.
    payload-red = red.
    payload-green = green.
    payload-blue = blue.

    DATA(lv_uri) = |rgb|.

    DATA(json) = /ui2/cl_json=>serialize( data = payload pretty_name = /ui2/cl_json=>pretty_mode-low_case ).

    cl_http_client=>create_by_destination( EXPORTING destination = 'IOT' IMPORTING client = DATA(request) ).

    cl_http_utility=>set_request_uri( EXPORTING request = request->request  uri = lv_uri ).
    request->request->set_header_field( EXPORTING name  = '~request_method' value = 'POST' ).
    request->request->set_header_field( EXPORTING name  = 'Content-Type' value = 'application/json' ).
    request->request->set_header_field( EXPORTING name  = 'api_key' value = '4iROJewnqAX6E59C9BXGQ7CWNAx8m5er' ).
    request->request->set_cdata( json ).

    request->send( ).
    request->receive( ).
    request->response->get_status( IMPORTING code = response-http_status_code reason = response-http_status_text ).

    refresh_data( ).
    alv->refresh( ).
  ENDMETHOD.
  METHOD turn_off_switch.
    DATA response TYPE ty_response.
    DATA payload TYPE ty_switch.
    payload-switch = 'Off'.

    DATA(lv_uri) = |switch|.

    DATA(json) = /ui2/cl_json=>serialize( data = payload pretty_name = /ui2/cl_json=>pretty_mode-low_case ).

    cl_http_client=>create_by_destination( EXPORTING destination = 'IOT' IMPORTING client = DATA(request) ).

    cl_http_utility=>set_request_uri( EXPORTING request = request->request  uri = lv_uri ).
    request->request->set_header_field( EXPORTING name  = '~request_method' value = 'POST' ).
    request->request->set_header_field( EXPORTING name  = 'Content-Type' value = 'application/json' ).
    request->request->set_header_field( EXPORTING name  = 'api_key' value = '4iROJewnqAX6E59C9BXGQ7CWNAx8m5er' ).
    request->request->set_cdata( json ).

    request->send( ).
    request->receive( ).
    request->response->get_status( IMPORTING code = response-http_status_code reason = response-http_status_text ).

    refresh_data( ).
    alv->refresh( ).
  ENDMETHOD.

  METHOD turn_on_switch.
    DATA response TYPE ty_response.
    DATA payload TYPE ty_switch.
    payload-switch = 'On'.

    DATA(lv_uri) = |switch|.

    DATA(json) = /ui2/cl_json=>serialize( data = payload pretty_name = /ui2/cl_json=>pretty_mode-low_case ).


    cl_http_client=>create_by_destination( EXPORTING destination = 'IOT' IMPORTING client = DATA(request) ).

    cl_http_utility=>set_request_uri( EXPORTING request = request->request  uri = lv_uri ).
    request->request->set_header_field( EXPORTING name  = '~request_method' value = 'POST' ).
    request->request->set_header_field( EXPORTING name  = 'Content-Type' value = 'application/json' ).
    request->request->set_header_field( EXPORTING name  = 'api_key' value = '4iROJewnqAX6E59C9BXGQ7CWNAx8m5er' ).
    request->request->set_cdata( json ).

    request->send( ).
    request->receive( ).
    request->response->get_status( IMPORTING code = response-http_status_code reason = response-http_status_text ).

    refresh_data( ).
    alv->refresh( ).
  ENDMETHOD.

  METHOD refresh_data.
    INCLUDE: <icon>.
    DATA response TYPE ty_response.
    DATA lw_data LIKE LINE OF lt_data.
    FREE lt_data.

    DATA(lv_uri) = |status|.

    cl_http_client=>create_by_destination( EXPORTING destination = 'IOT' IMPORTING client = DATA(request) ).

    cl_http_utility=>set_request_uri( EXPORTING request = request->request  uri = lv_uri ).
    request->request->set_header_field( EXPORTING name  = '~request_method' value = 'GET' ).
    request->request->set_header_field( EXPORTING name  = 'api_key' value = '4iROJewnqAX6E59C9BXGQ7CWNAx8m5er' ).

    request->send( ).
    request->receive( ).
    request->response->get_status( IMPORTING code = response-http_status_code reason = response-http_status_text ).
    request->response->get_cdata( RECEIVING data = response-content ).

    IF response-http_status_code = 200.
      /ui2/cl_json=>deserialize( EXPORTING json = response-content CHANGING data =  lw_data ).
      IF lw_data-switch = 'On'.
        lw_data-switch = icon_green_light.
      ELSE.
        lw_data-switch = icon_red_light.
      ENDIF.
      APPEND lw_data TO lt_data.

    ELSE.
      WRITE 'Error while retrieving data'.
    ENDIF.

  ENDMETHOD.

  METHOD display.
    DATA column TYPE REF TO cl_salv_column_table.


    TRY.
        cl_salv_table=>factory( IMPORTING r_salv_table = alv CHANGING t_table = lt_data ).

        DATA(events) = alv->get_event( ).

        DATA(lcl_events) = NEW lcl_handle_events( me ).
        SET HANDLER lcl_events->on_user_command FOR events.

        alv->set_screen_status( EXPORTING pfstatus = 'STANDARD' report = 'Z_IOT_CONTROLLER' set_functions = alv->c_functions_all ).

        DATA(functions) = alv->get_functions( ).
        functions->set_all( abap_true ).

        DATA(columns) = alv->get_columns( ).
        column ?= columns->get_column( 'SWITCH' ).
        column->set_icon( if_salv_c_bool_sap=>true ).
        column->set_long_text( 'Switch' ).
        column->set_alignment( if_salv_c_alignment=>centered ).
        column->set_output_length( 5 ).

        column ?= columns->get_column( 'RGB_LED-COLOR-RED' ).
        column->set_long_text( 'Red' ).
        column->set_short_text( 'Red' ).
        column->set_medium_text( 'Red' ).
        column->set_output_length( 5 ).

        column ?= columns->get_column( 'RGB_LED-COLOR-GREEN' ).
        column->set_long_text( 'Green' ).
        column->set_short_text( 'Green' ).
        column->set_medium_text( 'Green' ).
        column->set_output_length( 5 ).

        column ?= columns->get_column( 'RGB_LED-COLOR-BLUE' ).
        column->set_long_text( 'Blue' ).
        column->set_short_text( 'Blue' ).
        column->set_medium_text( 'Blue' ).
        column->set_output_length( 5 ).

        column ?= columns->get_column( 'RGB_LED-BRIGHTNESS' ).
        column->set_long_text( 'Brightness' ).
        column->set_short_text( 'Brightness' ).
        column->set_medium_text( 'Brightness' ).
        column->set_output_length( 8 ).


        alv->display( ).
      CATCH cx_salv_not_found.
      CATCH cx_salv_msg.
    ENDTRY.
  ENDMETHOD.

  METHOD get_current_switch.
    IF lt_data[ 1 ]-switch = icon_green_light.
      switch = 1.
    ELSE.
      switch = 0.
    ENDIF.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.

  DATA(iot) = NEW iot_controller( ).
  iot->refresh_data( ).
  iot->display( ).
