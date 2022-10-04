**----------------------------------------------------------------------
**   INCLUDE ZXLTOU01
**----------------------------------------------------------------------


CONSTANTS: c_nltyp(3)   VALUE '100',
           c_lgnum1(3)  VALUE '305',
           c_lgnum2(3)  VALUE '335',
           c_lgnum3(3)  VALUE '340',
           c_status1(2) VALUE '01',
           c_status2(2) VALUE '03'.

* data for  PACK statement.
DATA: d_vlenr(12)     TYPE n,
      d_counter       TYPE i,
*          d_seq(2) type c,
      d_seq           LIKE ztafr_status-status_seq,
      d_label_type    TYPE c,
      d_add_identi(4) TYPE n.

DATA : i_ztafr_l_status LIKE ztafr_l_status OCCURS 0 WITH HEADER LINE.
DATA : sztafr_l_status LIKE ztafr_l_status.


LOOP AT t_ltap_vb .
  PACK t_ltap_vb-vlenr TO d_vlenr.
* SELECTED KEY FIELDS FROM ZTAFR_L_HEADER.
  SELECT SINGLE label_type add_identi INTO (d_label_type ,
                                            d_add_identi)
  FROM  ztafr_l_header WHERE plant = t_ltap_vb-werks AND
                             label_no = d_vlenr.
* WEATHER A RECORD IS EXISTED OR NOT WITH SPECIED KEY VALUES
  SELECT  *  FROM ztafr_l_status INTO TABLE i_ztafr_l_status
                    WHERE plant = t_ltap_vb-werks  AND
                          label_type = d_label_type AND
                          label_no = d_vlenr AND
                          add_identi = d_add_identi.
  IF sy-subrc EQ 0.
*CHECKED DESTINATION STORAGE(100) AND PLANTS(305,340,335)
    IF t_ltap_vb-nltyp EQ c_nltyp AND
       t_ltap_vb-lgnum EQ c_lgnum1 OR
       t_ltap_vb-lgnum EQ c_lgnum1 OR
       t_ltap_vb-lgnum EQ c_lgnum1.
      READ TABLE i_ztafr_l_status WITH KEY plant = t_ltap_vb-werks
                                       label_type = d_label_type
                                       label_no = d_vlenr
                                       add_identi = d_add_identi
                                        status = c_status1.
      IF sy-subrc = 0.
*CHECKED WEATHER SAME LABEL CONTATION WITH STATUS O3.
        READ TABLE i_ztafr_l_status WITH KEY
                                 plant = t_ltap_vb-werks
                                 label_type = d_label_type
                                 label_no = d_vlenr
                                 add_identi = d_add_identi
                                 status = c_status2.
        IF  sy-subrc NE 0.
*MAX COUNTER NUMBER IS SELECTED
          SELECT SINGLE MAX( counter ) INTO d_counter FROM
                                      ztafr_l_status WHERE
                                      plant = t_ltap_vb-werks AND
                                      label_type = d_label_type AND
                                       label_no = d_vlenr AND
                                       add_identi = d_add_identi.

* STATUS SEQUANCE NUMBER IS SELECTED FROM TABLE ZTAFR_STATUS
          SELECT SINGLE status_seq INTO d_seq FROM ztafr_status
                 WHERE label_type EQ d_label_type
                       AND label_status EQ c_status2.
*INSERTED THE RECORD INTO TABLE ZTAFR_L_STATUS
          ztafr_l_status-plant      = i_ztafr_l_status-plant.
          ztafr_l_status-label_type = i_ztafr_l_status-label_type.
          ztafr_l_status-label_no   = i_ztafr_l_status-label_no .
          ztafr_l_status-add_identi = i_ztafr_l_status-add_identi.
          ztafr_l_status-status     = c_status2.
          ztafr_l_status-counter    = d_counter + 1.
          ztafr_l_status-status_seq = d_seq.
          ztafr_l_status-created_on = i_ztafr_l_status-created_on.
          ztafr_l_status-created_at = i_ztafr_l_status-created_at.
          ztafr_l_status-created_by = i_ztafr_l_status-created_by.
          ztafr_l_status-updated_by = i_ztafr_l_status-updated_by.
          INSERT ztafr_l_status.
          CLEAR ztafr_l_status.
        ENDIF.
      ENDIF.
    ENDIF.
  ELSE.
    EXIT.
  ENDIF.
ENDLOOP.
CLEAR i_ztafr_l_status.
REFRESH i_ztafr_l_status.
*{   INSERT         D01K923412                                        1
* >>> Martin Wagner, 27/05/2008 - Label Printing at event 04
*
DATA: ls_ltak          TYPE ltak,
      lv_logic_version TYPE  zzsd_lctrl_prt_lvers.

IF i_ltak_vb-kquit EQ 'X'   OR  i_ltak_vb-kvqui EQ 'X'.
  IF i_ltak_vb-bwlvs EQ '601'  OR  i_ltak_vb-bwlvs EQ '255'.

    MOVE-CORRESPONDING i_ltak_vb TO ls_ltak.

    CALL FUNCTION 'Z_SD_LABCTRL_GET_VERSION'
      EXPORTING
        ltak       = ls_ltak
        levent     = '04'  "at TO confirmation
      IMPORTING
        logic_vers = lv_logic_version.

    CASE lv_logic_version.
      WHEN '01'.
      WHEN '02'.
        CALL FUNCTION 'Z_HU_UM_04'
          EXPORTING
            ltak = i_ltak_vb
          TABLES
            ltap = t_ltap_vb.
      WHEN '03'.
      WHEN OTHERS.
    ENDCASE.
  ENDIF.
ENDIF.

* >>> Martin Wagner, 26/05/2009 - Agility: 1:1 Repacking
**** This Exit should not been processed here any more;
*Instead it is processed in front of "BOX_LABEL_PRINTING_NEW" in Function Module ZTAFR_L_TO_CONFIRM
IF i_ltak_vb-lgnum = '49A' AND i_ltak_vb-bwlvs = '991'.

  CALL FUNCTION 'Z_SD_2SPICK_REPACK' "IN UPDATE TASK
    EXPORTING
      ltak  = i_ltak_vb
      event = '02'
    TABLES
      ltap  = t_ltap_vb.

ENDIF.
*Begin of Insertion D02K941540
DATA: gt_vekp   TYPE TABLE OF vekp,
      gt_vekp1  TYPE TABLE OF vekp,
      lt_small  TYPE TABLE OF vekp,
      lt_ltap   TYPE TABLE OF ltap,
      ls_serial TYPE zsd_hyundai_hu.
DATA: label_data TYPE vwahn,
*      ls_note    TYPE zhu_note,
      ls_tabix   TYPE sy-tabix.
*READ TABLE
IF sy-tcode <> 'LT15' AND  sy-tcode <> 'LT16'.
  CLEAR: nast, tnapr.
  SELECT SINGLE *
    FROM likp
    INTO @DATA(ls_likp)
    WHERE vbeln = @t_ltap_vb-vbeln.
  SELECT SINGLE *
FROM zmaintain_print
INTO @DATA(ls_main)
WHERE parnr = @ls_likp-kunnr
AND parvw = 'WE'
AND werks = @t_ltap_vb-werks
AND object1 = 'SERIAL_NUMBER'
AND deletion = ' '.
  IF sy-subrc = 0.
    LOOP AT t_ltap_vb INTO DATA(ls_mix1).
      IF ls_mix1-nlenr IS INITIAL.
        APPEND ls_mix1 TO lt_ltap.
      ELSEIF ls_mix1-nlenr <> ls_mix1-vlenr.
        ls_mix1-vlenr = ls_mix1-nlenr.
        APPEND ls_mix1 TO lt_ltap.
      ELSEIF ls_mix1-nlenr = ls_mix1-vlenr.
        APPEND ls_mix1 TO lt_ltap.
      ENDIF.
      CLEAR ls_mix1.
    ENDLOOP.

    IF lt_ltap[] IS NOT INITIAL.
      SELECT *
        FROM vekp
        INTO TABLE @DATA(lt_vekp1)
        FOR ALL ENTRIES IN @lt_ltap
        WHERE exidv = @lt_ltap-vlenr.
    ENDIF.
    IF lt_vekp1 IS NOT INITIAL.
      LOOP AT lt_ltap INTO DATA(ltap2).
        READ TABLE lt_vekp1 ASSIGNING FIELD-SYMBOL(<vekp1>) WITH KEY exidv = ltap2-vlenr.
        IF <vekp1> IS ASSIGNED.
          <vekp1>-vbeln_gen = ltap2-nlpla.
          <vekp1>-posnr_gen = ltap2-posnr.
        ENDIF.
        CLEAR ltap2.
      ENDLOOP.
      gt_vekp = lt_vekp1.
      SELECT *
        FROM vepo
        INTO TABLE @DATA(lt_vepo1)
        FOR ALL ENTRIES IN @lt_vekp1
        WHERE venum = @lt_vekp1-venum.
      DELETE lt_vepo1 WHERE unvel IS INITIAL.
      IF lt_vepo1 IS NOT INITIAL.
        SELECT *
          FROM vekp
          APPENDING TABLE lt_vekp1
          FOR ALL ENTRIES IN lt_vepo1
          WHERE venum = lt_vepo1-unvel.
      ENDIF.
      LOOP AT gt_vekp INTO DATA(ls_vekp2).
*        APPEND ls_vekp2 TO lt_small.
        LOOP AT lt_vepo1 INTO DATA(ls_vepo2) WHERE venum = ls_vekp2-venum.
          READ TABLE lt_vekp1 ASSIGNING FIELD-SYMBOL(<vekpt>) WITH KEY venum = ls_vepo2-unvel.
          IF <vekpt> IS ASSIGNED.
            <vekpt>-vbeln_gen = ls_vekp2-vbeln_gen.
            <vekpt>-posnr_gen = ls_vekp2-posnr_gen.
            APPEND <vekpt> TO lt_small.
          ENDIF.
          CLEAR ls_vepo2.
        ENDLOOP.
        CLEAR ls_vekp2.
      ENDLOOP.
    ENDIF.
    CLEAR: gt_vekp, gt_vekp1, lt_ltap.
    SELECT MAX( serial )
            FROM zsd_hyundai_hu
            INTO @DATA(lv_num)
            WHERE lfdat = @ls_likp-lfdat
            AND kunnr = @ls_likp-kunnr.
    SELECT *
      FROM zsd_hyundai_hu
      INTO TABLE @DATA(lt_hu1)
      WHERE werks = @t_ltap_vb-werks
      AND tanum = @i_ltak_vb-tanum
      AND vbeln = @t_ltap_vb-vbeln
      AND lfdat = @ls_likp-lfdat
      AND kunnr = @ls_likp-kunnr.
    LOOP AT lt_small INTO DATA(ls_serl).
      DATA(ls_hu1) = VALUE #( lt_hu1[ exidv = ls_serl-exidv ] OPTIONAL ).
      IF ls_hu1 IS INITIAL.
        ls_hu1-tanum = i_ltak_vb-tanum.
        ls_hu1-werks = t_ltap_vb-werks.
        ls_hu1-vbeln = t_ltap_vb-vbeln.
        ls_hu1-lfdat = ls_likp-lfdat.
        ls_hu1-kunnr = ls_likp-kunnr.
        ls_hu1-exidv = ls_serl-exidv.
        lv_num = lv_num + 1.
        ls_hu1-serial = lv_num.
        ls_hu1-created_by = sy-uname.
        ls_hu1-created_on = sy-datum.
        ls_hu1-created_time = sy-uzeit.
        MODIFY zsd_hyundai_hu FROM ls_hu1.
        CLEAR: ls_hu1, ls_serl.
      ENDIF.
    ENDLOOP.
  ENDIF.
  CLEAR lt_small.
  SELECT SINGLE *
    FROM zmaintain_print
    INTO @DATA(ls_print1)
    WHERE parnr = @ls_likp-kunnr
    AND parvw = 'WE'
    AND werks = @t_ltap_vb-werks
    AND object1 = 'TO_PICK'
    AND deletion = ' '.
  IF sy-subrc = 0 .
*  CLEAR f_print.
*    SELECT SINGLE *
*    FROM tsp03d
*    INTO @DATA(ls_tsp03d)
*    WHERE name = @ls_print1-object2.
*  IF sy-subrc = 0.
*    lt_ltap = t_ltap_vb[].
    LOOP AT t_ltap_vb INTO DATA(ls_mix).
      IF ls_mix-nlenr IS INITIAL.
        APPEND ls_mix TO lt_ltap.
      ELSEIF ls_mix-nlenr <> ls_mix-vlenr.
          IF i_ltak_vb-kquit EQ 'X'.
            ls_mix-vlenr = ls_mix-nlenr.
            APPEND ls_mix TO lt_ltap.
          ENDIF.
      ELSEIF ls_mix-nlenr = ls_mix-vlenr.
        APPEND ls_mix TO lt_ltap.
      ENDIF.
      CLEAR ls_mix.
    ENDLOOP.
    SELECT *
      FROM zsd_to_hu_print
      INTO TABLE @DATA(lt_hu)
      WHERE lgnum = @i_ltak_vb-lgnum
      AND tanum = @i_ltak_vb-tanum.
*    AND object1 = 'TO_PICK'.
*    DELETE LT_LTAP WHERE PQUIT <> 'X'.
    IF lt_ltap[] IS NOT INITIAL.
      SELECT *
        FROM vekp
        INTO TABLE @DATA(lt_vekp)
        FOR ALL ENTRIES IN @lt_ltap
        WHERE exidv = @lt_ltap-vlenr.
    ENDIF.
    IF lt_vekp IS NOT INITIAL.
      LOOP AT lt_ltap INTO DATA(ltap1).
        READ TABLE lt_vekp ASSIGNING FIELD-SYMBOL(<vekp>) WITH KEY exidv = ltap1-vlenr.
        IF <vekp> IS ASSIGNED.
          <vekp>-vbeln_gen = ltap1-nlpla.
          <vekp>-posnr_gen = ltap1-posnr.
        ENDIF.
        CLEAR ltap1.
      ENDLOOP.
      gt_vekp = lt_vekp.
      SELECT *
        FROM vepo
        INTO TABLE @DATA(lt_vepo)
        FOR ALL ENTRIES IN @lt_vekp
        WHERE venum = @lt_vekp-venum.
      DELETE lt_vepo WHERE unvel IS INITIAL.
      IF lt_vepo IS NOT INITIAL.
        SELECT *
          FROM vekp
          APPENDING TABLE lt_vekp
          FOR ALL ENTRIES IN lt_vepo
          WHERE venum = lt_vepo-unvel.
      ENDIF.
      SELECT *
    FROM b904
    INTO TABLE @DATA(lt_b904)
    FOR ALL ENTRIES IN @lt_vekp
    WHERE kappl = 'V6'
    AND vstel = @ls_likp-vstel
    AND kunwe = @ls_likp-kunnr
    AND vhilm = @lt_vekp-vhilm.
      IF lt_b904 IS NOT INITIAL.
        SELECT knumh,
               ldest
               FROM nach
               INTO TABLE @DATA(lt_nach)
               FOR ALL ENTRIES IN @lt_b904
               WHERE knumh = @lt_b904-knumh.

        SELECT *
          FROM tnapr
          INTO TABLE @DATA(lt_tnapr)
          FOR ALL ENTRIES IN @lt_b904
          WHERE kschl = @lt_b904-kschl
          AND nacha = '1'
          AND kappl = 'V6'.
      ENDIF.
      LOOP AT gt_vekp INTO DATA(ls_vekp1).
        IF ls_main IS INITIAL.
          APPEND ls_vekp1 TO lt_small.
        ENDIF.
        LOOP AT lt_vepo INTO DATA(ls_vepo) WHERE venum = ls_vekp1-venum.
          READ TABLE lt_vekp ASSIGNING <vekp> WITH KEY venum = ls_vepo-unvel.
          IF <vekp> IS ASSIGNED.
            <vekp>-vbeln_gen = ls_vekp1-vbeln_gen.
            <vekp>-posnr_gen = ls_vekp1-posnr_gen.
            APPEND <vekp> TO lt_small.
          ENDIF.
          CLEAR ls_vepo.
        ENDLOOP.
        CLEAR ls_vekp1.
      ENDLOOP.
    ENDIF.
*  ENDIF.

*    IF sy-subrc = 0.
*
*    ENDIF.
    CLEAR gt_vekp1.
    IF ls_print1-value1 = 'MASTER'.
*    gt_vekp1 = gt_vekp.
      LOOP AT gt_vekp INTO DATA(gs_vekp).
        ls_tabix = sy-tabix.
        CALL FUNCTION 'PACKING_LABEL_READ'
          EXPORTING
            shipping_unit = gs_vekp-venum
*           partner_number   = nast-parnr
*           partner_function = nast-parvw
          IMPORTING
            label_data    = label_data
          EXCEPTIONS
            OTHERS        = 1.
        IF sy-subrc = 0.
          IF label_data-smgkn = 'M' OR label_data-smgkn = 'G' OR ( label_data-smgkn = 'S' AND gs_vekp-uevel IS INITIAL AND i_ltak_vb-lgnum = '902' ).
            DATA(ls_hu) = VALUE #( lt_hu[ lgnum = i_ltak_vb-lgnum tanum = i_ltak_vb-tanum exidv = gs_vekp-exidv ] OPTIONAL ).
            IF ls_hu IS INITIAL.
              APPEND gs_vekp TO gt_vekp1.
            ENDIF.
          ENDIF.
        ENDIF.
        CLEAR: gs_vekp, label_data, ls_tabix, ls_hu.
      ENDLOOP.
    ELSE.
*    gt_vekp1 = lt_vekp.
      LOOP AT lt_small INTO gs_vekp.
        ls_hu = VALUE #( lt_hu[ lgnum = i_ltak_vb-lgnum tanum = i_ltak_vb-tanum exidv = gs_vekp-exidv ] OPTIONAL ).
        IF ls_hu IS INITIAL.
          ls_vepo = VALUE #( lt_vepo[ unvel = gs_vekp-venum ] OPTIONAL ).
          IF ls_vepo IS INITIAL.
            APPEND gs_vekp TO gt_vekp1.
          ELSE.
            ls_vekp1 = VALUE #( gt_vekp[ venum = ls_vepo-venum ] OPTIONAL ).
            IF ls_vekp1 IS NOT INITIAL.
              DATA(ls_hug) = VALUE #( lt_hu[ lgnum = i_ltak_vb-lgnum tanum = i_ltak_vb-tanum exidv = ls_vekp1-exidv ] OPTIONAL ).
              IF ls_hug IS INITIAL.
                APPEND gs_vekp TO gt_vekp1.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.
        CLEAR: ls_hu, gs_vekp.
      ENDLOOP.

    ENDIF.
    DELETE ADJACENT DUPLICATES FROM gt_vekp1 COMPARING ALL FIELDS.
    LOOP AT gt_vekp1 INTO DATA(ls_vekp).
      DATA(ls_b904) = VALUE #( lt_b904[ vhilm = ls_vekp-vhilm ] OPTIONAL ).
      IF ls_b904 IS NOT INITIAL.
        IF ls_b904-kschl <> 'ZSRB' AND ls_b904-kschl <> 'ZSNB'.
          vekp = ls_vekp.
          DATA(ls_tnapr) = VALUE #( lt_tnapr[ kschl = ls_b904-kschl ] OPTIONAL ).
          IF ls_tnapr IS NOT INITIAL.
            nast-objky = ls_vekp-venum.
            DATA(ls_nach) = VALUE #( lt_nach[ knumh = ls_b904-knumh ] OPTIONAL ).
            IF ls_nach IS NOT INITIAL.
              nast-ldest      = ls_nach-ldest.
            ENDIF.
            nast-spras = 'E'.
            nast-parvw = 'WE'.
            nast-parnr = ls_likp-kunnr.
            nast-dsuf2      = '    '.
            nast-dimme = 'X'.
            tnapr-sform = ls_tnapr-sform.
            nast-kschl = ls_b904-kschl.
            nast-kappl = 'V6'.
            PERFORM (ls_tnapr-ronam) IN PROGRAM (ls_tnapr-pgnam) USING
            ' ' ''.
            CLEAR ls_hu.
            ls_hu-lgnum = i_ltak_vb-lgnum.
            ls_hu-tanum = i_ltak_vb-tanum.
            ls_hu-exidv = ls_vekp-exidv.
            ls_hu-object1 = 'TO_PICK'.
            ls_hu-created_by = sy-uname.
            ls_hu-created_on = sy-datum.
            ls_hu-created_time = sy-uzeit.
            MODIFY zsd_to_hu_print FROM ls_hu.
          ENDIF.
        ELSEIF ls_b904-kschl = 'ZSRB' OR ls_b904-kschl = 'ZSNB'.
          ls_tnapr = VALUE #( lt_tnapr[ kschl = ls_b904-kschl ] OPTIONAL ).
          IF ls_tnapr IS NOT INITIAL.
            nast-objky = ls_vekp-venum.
            DATA(ls_nach1) = VALUE #( lt_nach[ knumh = ls_b904-knumh ] OPTIONAL ).
            IF ls_nach1 IS NOT INITIAL.
              nast-ldest      = ls_nach1-ldest.
            ENDIF.
            nast-spras = 'E'.
            nast-dsuf2      = '    '.
            nast-dimme = 'X'.
            tnapr-sform = ls_tnapr-sform.
            nast-kschl = ls_b904-kschl.
            nast-kappl = 'V6'.
            nast-optarcnr = i_ltak_vb-tanum.
            PERFORM entry IN PROGRAM zsd_all_um_eti9_report_to
            TABLES gt_vekp1
            USING
            ' ' ''.
            LOOP AT gt_vekp1 INTO gs_vekp.
              CLEAR ls_hu.
              ls_hu-lgnum = i_ltak_vb-lgnum.
              ls_hu-tanum = i_ltak_vb-tanum.
              ls_hu-exidv = gs_vekp-exidv.
              ls_hu-object1 = 'TO_PICK'.
              ls_hu-created_by = sy-uname.
              ls_hu-created_on = sy-datum.
              ls_hu-created_time = sy-uzeit.
              MODIFY zsd_to_hu_print FROM ls_hu.
              CLEAR gs_vekp.
            ENDLOOP.
            EXIT.
          ENDIF.
*        EXIT.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDIF.
ENDIF.
CLEAR: gt_vekp, lt_vekp, lt_vepo, gt_vekp1, lt_b904, lt_hu, lt_small,ls_main, nast, tnapr, vekp.
*End of Insertion D02K941540
*}   INSERT
