*----------------------------------------------------------------------*
*   INCLUDE ZXLTOU01                                                   *
*----------------------------------------------------------------------*
INCLUDE zxltou01_doc.
INCLUDE zxltou01_inc.

TABLES : ztafr_l_plant,
         ltap,
         ltbp,
         nast,
         *ltap,
         vbfa,
         tnapr,
*         vepo,
*         vekp,
         vevw,
         humseg,
         t333.


DATA : sysubrc LIKE sy-subrc.
DATA: it_lqua LIKE lqua .

DATA: v_lifnr     LIKE ztafr_edi_intcom-lifnr,
      v_lifnr2    LIKE ztafr_edi_intcom-lifnr,
      v_werks     LIKE likp-werks,
      v_mhdhb     LIKE mara-mhdhb,
      v_bin       LIKE ztafr_edi_intcom-bin_ind,
      lv_updvfdat TYPE zupdat.

DATA: lv_logic_version TYPE zzsd_lctrl_prt_lvers,
      ls_ltak_ctrl     TYPE ltak.

CLEAR : lheader, iztafr_l_param, label_table,
        error_table.
REFRESH : lheader, iztafr_l_param, label_table,
          error_table.

*******************************************************************************
* I.1. 7 different scenarios where update of LTAP and / or LQUA is neccessary *
*******************************************************************************

LOOP AT t_ltap_vb.
* 1. case                                                               1. case
  IF ( t_ltap_vb-lgnum+0(1) = '3'
    OR t_ltap_vb-lgnum+0(3) = '401' )
     AND ( ( t_ltap_vb-bwlvs LT '921'
     OR t_ltap_vb-bwlvs GT '954' )
     AND ( ( t_ltap_vb-nltyp = '100'
     AND t_ltap_vb-bestq NE ' ' )
     OR ( t_ltap_vb-nltyp = '101'
     AND t_ltap_vb-bestq NE ' ' )
     OR ( t_ltap_vb-nltyp = '102'
     AND t_ltap_vb-bestq NE ' ' ) ) ).
    MESSAGE e033(l5).
*   Dieser Vorgang ist im Lean-WM nicht vorgesehen
    EXIT.
  ELSE.
* 2. case                                                               2. case
************************************************************************
** mark flag in LTAP field NPLEI in transfer order in combination with *
** WM mov. type 916 -> Dest.stor.bin changeable during confirmation.   *
** In combination with customizing OMLX for destination storage type   *
** this allows us to confirm storage unit transfers with the GUN       *
** Witte 27.05.2009                                                    *
************************************************************************
    IF t_ltap_vb-lgnum = '400'
      AND t_ltap_vb-bwlvs EQ '917'.
      SELECT * FROM ltap WHERE lgnum = t_ltap_vb-lgnum
                           AND tanum = t_ltap_vb-tanum
                           AND tapos = t_ltap_vb-tapos.
        IF sy-subrc = 0.
          ltap-nplei = 'X'.
          MODIFY ltap.
        ENDIF.
      ENDSELECT.
    ELSE.
* 3. case                                                               3. case
************************************************************************
** mark flag in LTAP field NPLEI in transfer order in combination with *
** WM mov. type 916 -> Dest.stor.bin changeable during confirmation.   *
** In combination with customizing OMLX for destination storage type   *
** this allows us to confirm storage unit transfers with the GUN       *
** Witte 27.05.2009                                                    *
************************************************************************
      IF t_ltap_vb-lgnum = '305'
        AND t_ltap_vb-bwlvs EQ '916'.
        SELECT * FROM ltap WHERE lgnum = t_ltap_vb-lgnum
                             AND tanum = t_ltap_vb-tanum
                             AND tapos = t_ltap_vb-tapos.
          IF sy-subrc = 0.
            ltap-nplei = 'X'.
            MODIFY ltap.
          ENDIF.
        ENDSELECT.
      ELSE.
* 4. case                                                               4. case
************************************************************************
** To update div fields in LTAP and LQUA in case of inbound delivery   *
** transfer postings from 0004 -> 0001                                 *
** Witte 12.05.2009                                                    *
************************************************************************

        IF t_ltap_vb-bwlvs EQ '298' OR
            t_ltap_vb-bwlvs EQ '299'.

          CLEAR ltap.
          CLEAR i_vepo. REFRESH i_vepo.
          CLEAR t_um. REFRESH t_um.
          CLEAR t_header. REFRESH t_header.
          CLEAR t_item. REFRESH t_item.


          SELECT * FROM ltap WHERE lgnum = t_ltap_vb-lgnum
                                        AND tanum = t_ltap_vb-tanum
                                        AND tapos = t_ltap_vb-tapos.
            IF ltap-nlenr CO '0123456789'.

              SELECT SINGLE * FROM vekp WHERE exidv = ltap-nlenr.
              IF sy-subrc = 0.
                SELECT SINGLE * FROM  vepo INTO i_vepo
                                WHERE venum = vekp-venum
                                  AND velin = '1'.
                IF sy-subrc = 0.
                  ltap-qplos = i_vepo-qplos.
                  ltap-qploa = i_vepo-qplos.
                  ltap-vbeln = i_vepo-vbeln.
                  SELECT * FROM vbfa WHERE vbelv   = i_vepo-vbeln
                                     AND posnv   = i_vepo-posnr
                                     AND vbtyp_n = 'R'.
                    IF sy-subrc = 0.
                      ltap-wenum = vbfa-vbeln.
                      ltap-wepos = vbfa-posnn.
                    ENDIF.
                  ENDSELECT.
                ELSE.
                  t_um-hu_exid = ltap-nlenr.
                  APPEND t_um.
                  CALL FUNCTION 'BAPI_HU_GETLIST'
                    EXPORTING
                      notext    = ' '
                      onlykeys  = ' '
                    TABLES
                      hunumbers = t_um
*                     HUOBJECTS =
*                     HUKEY     =
                      huheader  = t_header
                      huitem    = t_item
*                     HUHISTORY =
*                     NOTFOUNDHUS        =
*                     HIGHESTLEVEL       =
                      return    = t_return.
                  IF sy-subrc = 0.
                    LOOP AT t_item WHERE hu_item_type = '1'
                                     AND material = ltap-matnr.
                      ltap-qplos = t_item-insplot.
                      ltap-qploa = t_item-insplot.
                      ltap-vbeln = t_item-object_doc.
                      i_vepo-vbeln = t_item-object_doc.
                      i_vepo-posnr = t_item-obj_item_number.
                    ENDLOOP.
                    SELECT * FROM vbfa WHERE vbelv   = i_vepo-vbeln
                                       AND posnv   = i_vepo-posnr
                                       AND vbtyp_n = 'R'.
                      IF sy-subrc = 0.
                        ltap-wenum = vbfa-vbeln.
                        ltap-wepos = vbfa-posnn.
                      ENDIF.
                    ENDSELECT.
                  ENDIF.
                ENDIF.
                MODIFY ltap.
              ENDIF.
            ENDIF.
          ENDSELECT.

          SELECT * FROM lqua INTO it_lqua WHERE lgnum = t_ltap_vb-lgnum
                                            AND lqnum = t_ltap_vb-nlqnr.

            SELECT SINGLE * FROM vekp WHERE exidv = it_lqua-lenum.
            IF sy-subrc = 0.
              SELECT SINGLE * FROM  vepo INTO i_vepo
                              WHERE venum = vekp-venum
                                AND velin = '1'.
              IF sy-subrc = 0.
                it_lqua-qplos = i_vepo-qplos.
                it_lqua-vbeln = i_vepo-vbeln.
                it_lqua-posnr = i_vepo-posnr.
                SELECT * FROM vbfa WHERE vbelv   = i_vepo-vbeln
                                   AND posnv   = i_vepo-posnr
                                   AND vbtyp_n = 'R'.
                  IF sy-subrc = 0.
                    it_lqua-wenum = vbfa-vbeln.
                    it_lqua-wepos = vbfa-posnn.
                  ENDIF.
                ENDSELECT.
              ELSE.
                t_um-hu_exid = it_lqua-lenum.
                APPEND t_um.
                CALL FUNCTION 'BAPI_HU_GETLIST'
                  EXPORTING
                    notext    = ' '
                    onlykeys  = ' '
                  TABLES
                    hunumbers = t_um
*                   HUOBJECTS =
*                   HUKEY     =
                    huheader  = t_header
                    huitem    = t_item
*                   HUHISTORY =
*                   NOTFOUNDHUS        =
*                   HIGHESTLEVEL       =
                    return    = t_return.
                IF sy-subrc = 0.
                  LOOP AT t_item WHERE hu_item_type = '1'
                                   AND material = it_lqua-matnr.
                    it_lqua-qplos = t_item-insplot.
                    it_lqua-vbeln = t_item-object_doc.
                    i_vepo-posnr = t_item-obj_item_number.
                    i_vepo-vbeln = t_item-object_doc.
                    it_lqua-posnr = t_item-obj_item_number.
                  ENDLOOP.
                  SELECT * FROM vbfa WHERE vbelv   = i_vepo-vbeln
                                       AND posnv   = i_vepo-posnr
                                       AND vbtyp_n = 'R'.
                    IF sy-subrc = 0.
                      it_lqua-wenum = vbfa-vbeln.
                      it_lqua-wepos = vbfa-posnn.
                    ENDIF.
                  ENDSELECT.
                ENDIF.
              ENDIF.
              UPDATE lqua FROM it_lqua.
            ENDIF.
          ENDSELECT.
        ELSE.
* 5. case                                                               5. case
************************************************************************
** To update LTAP-WDATU & LTAP-WENUM & LTAP-WEPOS & LTAP-ZEUGN         *
** LQUA-WDATU & LQUA-WENUM & LQUA-WEPOS & LQUA-ZEUGN                   *
** Witte 25.02.2010                                                    *
************************************************************************

          IF t_ltap_vb-bwlvs EQ '199'
            AND t_ltap_vb-nlenr CO '0123456789'.

            CLEAR i_ztafr_l_header. REFRESH i_ztafr_l_header.
            CLEAR i_vekp. REFRESH i_vekp.
            CLEAR i_vepo. REFRESH i_vepo.
            CLEAR i_humseg. REFRESH i_humseg.
            CLEAR i_mseg. REFRESH i_mseg.
            CLEAR i_mkpf. REFRESH i_mkpf.
            CLEAR i_ltap1. REFRESH i_ltap1.

* Lagereinheit in Label Table enthalten?
            SELECT * FROM ztafr_l_header INTO TABLE i_ztafr_l_header
                    WHERE plant    = t_ltap_vb-werks
                      AND label_no = t_ltap_vb-nlenr.
            IF sy-subrc = 0.
              READ TABLE i_ztafr_l_header INDEX 1.
* HU-Daten bestimmen sofern vorhanden
              SELECT * FROM vekp INTO TABLE i_vekp
                      WHERE exidv  = t_ltap_vb-nlenr.
              IF sy-subrc = 0.
                READ TABLE i_vekp INDEX 1.
*  Lesen erste Handling Unit Position
                SELECT * FROM vepo INTO TABLE i_vepo
                        WHERE venum = i_vekp-venum.
                IF sy-subrc = 0.
                  READ TABLE i_vepo INDEX 1.
                  IF i_vepo-velin = 1.
                    h_venum = i_vekp-venum.
                  ELSE.
                    h_venum = i_vepo-unvel.
                  ENDIF.
                  SELECT * FROM humseg INTO TABLE i_humseg
                          WHERE venum = h_venum
                            AND mblnr = i_ztafr_l_header-refdoc_no2.
                  IF sy-subrc = 0.
                    READ TABLE i_humseg INDEX 1.
                  ENDIF.
                ENDIF.
              ENDIF.
              IF i_ztafr_l_header-operation_no NE '    '.
                SELECT * FROM ltap INTO TABLE i_ltap1
                        WHERE tanum = i_ztafr_l_header-refdoc_no
                          AND nlenr = t_ltap_vb-nlenr
                          AND lgnum = t_ltap_vb-lgnum.
                IF sy-subrc = 0.
                  READ TABLE i_ltap1 INDEX 1.
                ENDIF.
                SELECT * FROM  mseg INTO TABLE i_mseg
                        WHERE mblnr = i_ltap1-wenum
                          AND zeile = i_ltap1-wepos.
                IF sy-subrc = 0.
                  READ TABLE i_mseg INDEX 1.
                  SELECT * FROM mkpf INTO TABLE i_mkpf
                          WHERE mblnr = i_mseg-lfbnr
                            AND mjahr = i_mseg-mjahr.
                  IF sy-subrc = 0.
                    READ TABLE i_mkpf INDEX 1.
                  ENDIF.
                ENDIF.
              ELSE.
                SELECT * FROM  mseg INTO TABLE i_mseg
                        WHERE mblnr = i_humseg-mblnr
                          AND zeile = i_humseg-mblpo.
                IF sy-subrc = 0.
                  READ TABLE i_mseg INDEX 1.
                  SELECT * FROM mkpf INTO TABLE i_mkpf
                          WHERE mblnr = i_mseg-lfbnr
                            AND mjahr = i_mseg-mjahr.
                  IF sy-subrc = 0.
                    READ TABLE i_mkpf INDEX 1.
                  ENDIF.
                ENDIF.
              ENDIF.


              CLEAR ltap.

              SELECT * FROM  ltap
                       WHERE lgnum = t_ltap_vb-lgnum
                         AND tanum = t_ltap_vb-tanum
                         AND tapos = t_ltap_vb-tapos.
                IF sy-subrc = 0.
                  ltap-wenum = i_mseg-lfbnr.
                  ltap-wepos = i_mseg-lfpos.
                  ltap-wdatu = i_mkpf-budat.
                  ltap-zeugn = i_mseg-ablad+0(10).
                ENDIF.
                MODIFY ltap.
              ENDSELECT.

              SELECT * FROM lqua INTO it_lqua WHERE lgnum = t_ltap_vb-lgnum
                                                AND lqnum = t_ltap_vb-nlqnr.
                IF sy-subrc = 0.
                  it_lqua-wenum = i_mseg-lfbnr.
                  it_lqua-wepos = i_mseg-lfpos.
                  it_lqua-wdatu = i_mkpf-budat.
                  it_lqua-zeugn = i_mseg-ablad+0(10).
                ENDIF.
                UPDATE lqua FROM it_lqua.
              ENDSELECT.
            ENDIF.
* 199 Ende
          ELSE.
* 6. case                                                               6. case
************************************************************************
** To update LTAP-VFDAT & LTAP-HSDAT & LTAP-WENUM & LTAP-WEPOS &       *
** LQUA-VFDAT & LQUA-WDATU & LQUA-WENUM & LQUA-WEPOS from LTAP-ZEUGN   *
** Witte 05.01.2006                                                    *
************************************************************************

            IF t_ltap_vb-lgnum = '101' AND
              ( t_ltap_vb-bwlvs EQ '100' OR
                t_ltap_vb-bwlvs EQ '799' ).        " Witte Änderung fehlte!!

              CLEAR ltap.

              SELECT * FROM ltap WHERE lgnum = t_ltap_vb-lgnum
                                            AND tanum = t_ltap_vb-tanum
                                            AND tapos = t_ltap_vb-tapos.

                IF ltap-zeugn GE 0.

                  SELECT SINGLE * FROM mseg WHERE mblnr = ltap-zeugn
                                            AND matnr = ltap-matnr.
                  IF sy-subrc = 0.
*           ltap-vfdat = mseg-vfdat.
*           ltap-wdatu = mseg-hsdat.
                    ltap-wenum = mseg-lfbnr.
                    ltap-wepos = mseg-lfpos.
                    SELECT SINGLE * FROM ltbp WHERE lgnum = t_ltap_vb-lgnum
                                                AND tbnum = mseg-tbnum
                                                AND tbpos = mseg-tbpos.
                    IF sy-subrc = 0.
                      ltap-vfdat = ltbp-vfdat.
                      ltap-wdatu = ltbp-wdatu.
                    ENDIF.
                  ENDIF.
                ENDIF.
                MODIFY ltap.
              ENDSELECT.

              SELECT * FROM lqua INTO it_lqua WHERE lgnum = t_ltap_vb-lgnum
                                                AND lqnum = t_ltap_vb-nlqnr.

                SELECT SINGLE * FROM mseg WHERE mblnr = ltap-zeugn
                                          AND matnr = ltap-matnr.
                IF sy-subrc = 0.
*         it_lqua-vfdat = mseg-vfdat.
*         it_lqua-wdatu = mseg-hsdat.
                  it_lqua-wenum = mseg-lfbnr.
                  it_lqua-wepos = mseg-lfpos.
                  it_lqua-zeugn = ' '.
                  SELECT SINGLE * FROM ltbp WHERE lgnum = t_ltap_vb-lgnum
                                              AND tbnum = mseg-tbnum
                                              AND tbpos = mseg-tbpos.
                  IF sy-subrc = 0.
                    it_lqua-vfdat = ltbp-vfdat.
                    it_lqua-wdatu = ltbp-wdatu.
                  ENDIF.
                ENDIF.
                UPDATE lqua FROM it_lqua.

              ENDSELECT.
            ELSE.
* 7. case                                                               7. case
************************************************************************
** To update LTAP-VFDAT & LTAP-HSDAT & LTAP-WENUM & LTAP-WEPOS &       *
** LQUA-VFDAT & LQUA-WDATU & LQUA-WENUM & LQUA-WEPOS from LTAP-ZEUGN   *
** Witte 05.01.2006                                                    *
************************************************************************

*      Begin Changing M. Weber 2006.10.09
*      IF t_ltap_vb-lgnum = '335' and
*         t_ltap_vb-bwlvs eq '199'.

*      IF ( t_ltap_vb-lgnum = '335' AND
*           t_ltap_vb-bwlvs EQ '199' ) OR
*         ( t_ltap_vb-lgnum = '364' AND
*           t_ltap_vb-bwlvs EQ '907' ) .
*
**      End changing M. Weber
**      Begin changing MB 20090129
              IF ( t_ltap_vb-lgnum = '335' AND
                   t_ltap_vb-bwlvs EQ '199' ) OR
                 ( t_ltap_vb-lgnum = '364' AND
                   t_ltap_vb-bwlvs EQ '907' ) OR
                 ( t_ltap_vb-lgnum = '405' AND   "Inepsa
                   t_ltap_vb-bwlvs EQ '907' ) .  "Inepsa
**      End changing MB 20090129


                CLEAR ltap.

                SELECT * FROM ltap WHERE lgnum = t_ltap_vb-lgnum
                                              AND tanum = t_ltap_vb-tanum
                                              AND tapos = t_ltap_vb-tapos.
                  IF ltap-nlenr GE 0.
                    SELECT SINGLE * FROM vekp WHERE exidv = ltap-nlenr.
                    IF sy-subrc = 0.
                      SELECT  * FROM humseg
                              UP TO 1 ROWS
                                 WHERE venum = vekp-venum
                                   AND object = '09'
                        ORDER BY tstmp DESCENDING.
                      ENDSELECT.
                      IF sy-subrc = 0.
                        SELECT SINGLE * FROM mseg WHERE mblnr = humseg-mblnr
                                                    AND mjahr = humseg-mjahr
                                                    AND zeile = humseg-mblpo.
                        IF sy-subrc = 0.
                          ltap-vfdat = mseg-vfdat.
                          ltap-wdatu = mseg-hsdat.
                          ltap-wenum = mseg-mblnr.
                          ltap-wepos = mseg-zeile.
                          MODIFY ltap.
                        ENDIF.
                      ENDIF.
                    ENDIF.
                  ENDIF.
                ENDSELECT.

                SELECT * FROM lqua INTO it_lqua
                                   WHERE  lgnum = t_ltap_vb-lgnum
                                     AND  lqnum = t_ltap_vb-nlqnr.

                  IF sy-subrc = 0.
                    SELECT SINGLE * FROM mseg WHERE mblnr = ltap-wenum
                                                AND zeile = ltap-wepos
                                               AND matnr = ltap-matnr.
                    IF sy-subrc = 0.
                      it_lqua-vfdat = mseg-vfdat.
                      it_lqua-wdatu = mseg-hsdat.
                      it_lqua-wenum = mseg-mblnr.
                      it_lqua-wepos = mseg-zeile.
                      UPDATE lqua FROM it_lqua.
                    ENDIF.
                  ENDIF.
                ENDSELECT.
                IF ltap-nlenr GT '0'.
                  v_labelno =  ltap-nlenr+10(10).
                  CONCATENATE c_prefix v_labelno INTO lheader-label_no.
                  SELECT SINGLE * FROM ztafr_l_header
                                 WHERE plant EQ ltap-werks
                                   AND label_no EQ lheader-label_no.
                  IF sy-subrc EQ 0.
                    SELECT SINGLE * FROM vekp WHERE exidv = ltap-nlenr.
                    IF sy-subrc = 0.
                      SELECT  * FROM humseg
                              UP TO 1 ROWS
                                 WHERE venum = vekp-venum
                                   AND object = '09'
                        ORDER BY tstmp DESCENDING.
                      ENDSELECT.
                      IF sy-subrc = 0.
                        SELECT SINGLE * FROM mseg WHERE mblnr = humseg-mblnr
                                                    AND mjahr = humseg-mjahr
                                                    AND zeile = humseg-mblpo.
                        IF sy-subrc = '0'.
                          ztafr_l_header-qty_pack = mseg-menge.
                          ztafr_l_header-qty_unit  =  ltap-meins.
                          ztafr_l_header-weight  =  mseg-menge.
                          ztafr_l_header-exp_date  =  ltap-vfdat.
                          ztafr_l_header-refdoc_no2 = ltap-wenum.
                          UPDATE ztafr_l_header.
                          CLEAR ztafr_l_header.
                        ENDIF.
                      ENDIF.
                    ENDIF.
                  ENDIF.
                ENDIF.
              ELSE.
* 8. case                                                               8. case
************************************************************************
** To update LTAP-ZEUGN & LQUA-ZEUGN from LTAP-ABLAD                   *
** Gokul 11MAR2004                                                     *
** Assumption is that ZEUGN field is only for information and no       *
** other significance                                                  *
** Update only when source storage type = '902' (Goods Receipt)        *
** and the plant has VEND_BATCH active in table ZTAFR_L_PLANT.         *
************************************************************************

                IF t_ltap_vb-vltyp = '902'.

                  SELECT SINGLE * FROM ztafr_l_plant
                                WHERE plant = t_ltap_vb-werks
                                  AND vend_batch = 'X'.

                  IF sy-subrc = 0.

                    CLEAR ltap.

                    SELECT * FROM ltap WHERE lgnum = t_ltap_vb-lgnum
                                              AND tanum = t_ltap_vb-tanum
                                              AND tapos = t_ltap_vb-tapos.

                      IF ltap-zeugn IS INITIAL.
                        ltap-zeugn = ltap-ablad+0(10).
                      ENDIF.

** Set expiration date - Gokul 18MAY2004
** Change Request No:  WM-0043

                      IF NOT i_ltak_vb-vbeln IS INITIAL.

                        CLEAR:  v_lifnr,v_lifnr2,v_werks,v_mhdhb,v_bin,
                                lv_updvfdat.

                        SELECT SINGLE lifnr  INTO v_lifnr2
                                FROM likp WHERE vbeln =  i_ltak_vb-vbeln.
                        IF sy-subrc = 0.

                          SELECT SINGLE werks INTO v_werks FROM t320
                                       WHERE  lgnum = i_ltak_vb-lgnum.

                          SELECT SINGLE lifnr  bin_ind edi_upd_vfdat
                                     INTO (v_lifnr,v_bin, lv_updvfdat)
                                     FROM ztafr_edi_intcom
                                               WHERE lifnr  = v_lifnr2
                                              AND rwerks = v_werks .
                          IF sy-subrc = 0.
*  >>> Martin Wagner, August 2008: New Logic: lips-vfdat ----------- *
                            IF lv_updvfdat = 'A'.
                              SELECT SINGLE vfdat FROM lips INTO ltap-vfdat
                                     WHERE vbeln = i_ltak_vb-vbeln
                                     AND   posnr = ltap-posnr.
* <<<< Ende Wagner ------------------------------------------------- *
                            ELSE.
                              SELECT SINGLE mhdhb INTO v_mhdhb
                                         FROM mara
                                         WHERE matnr = ltap-matnr
                                         AND   mhdrz > 0.
                              IF sy-subrc = 0.
                                SELECT SINGLE * FROM vekp
                                       WHERE exidv =  ltap-nlenr.
                                IF sy-subrc = 0.
                                  IF v_bin = 'X'.
                                    IF v_mhdhb > 0.
                                      ltap-vfdat = vekp-erdat + v_mhdhb.
                                    ELSE.
                                      ltap-vfdat =  vekp-erdat + 30.
                                    ENDIF.
                                  ELSE.
                                    ltap-vfdat+0(2) = '20'.
                                    ltap-vfdat+2(6) = vekp-inhalt+0(6).
                                    IF v_mhdhb > 0.
                                      ltap-vfdat = ltap-vfdat + v_mhdhb.
                                    ELSE.
                                      ltap-vfdat = ltap-vfdat + 30.
                                    ENDIF.
                                  ENDIF.
                                ENDIF.
                              ENDIF.
                            ENDIF.
                          ENDIF.
                        ENDIF.
                      ENDIF.
** End - Gokul 18MAY2004

                      MODIFY ltap.


                    ENDSELECT.


                    SELECT * FROM lqua INTO it_lqua
                                       WHERE  lgnum = t_ltap_vb-lgnum
                                         AND  lqnum = t_ltap_vb-nlqnr.

                      IF it_lqua-zeugn IS INITIAL.
                        it_lqua-zeugn = t_ltap_vb-ablad+0(10).
                      ENDIF.
** Set expiration date Gokul 18MAY2004
** Change Request No:  WM-0043

                      CLEAR:  v_lifnr,v_lifnr2,v_werks,v_mhdhb, v_bin,
                              lv_updvfdat.

                      IF NOT i_ltak_vb-vbeln IS INITIAL.

                        SELECT SINGLE lifnr  INTO v_lifnr2
                                FROM likp WHERE vbeln =  i_ltak_vb-vbeln.
                        IF sy-subrc = 0.
                          SELECT SINGLE werks INTO v_werks FROM t320
                                      WHERE  lgnum = i_ltak_vb-lgnum.
                          SELECT SINGLE lifnr bin_ind edi_upd_vfdat
                                     INTO (v_lifnr,v_bin, lv_updvfdat)
                                     FROM  ztafr_edi_intcom
                                     WHERE lifnr  = v_lifnr2
                                     AND   rwerks = v_werks .
                          IF sy-subrc = 0.
*  >>> Martin Wagner, August 2008: New Logic: lips-vfdat ----------- *
                            IF lv_updvfdat = 'A'.
                              SELECT SINGLE vfdat FROM lips INTO it_lqua-vfdat
                                     WHERE vbeln = i_ltak_vb-vbeln
                                     AND   posnr = t_ltap_vb-posnr.
* <<<< Ende Wagner ------------------------------------------------- *
                            ELSE.
                              SELECT SINGLE mhdhb INTO v_mhdhb
                                     FROM mara
                                     WHERE matnr =  ltap-matnr
                                     AND   mhdrz > 0.
                              IF sy-subrc = 0.
                                SELECT SINGLE * FROM vekp
                                       WHERE exidv =  ltap-nlenr.
                                IF sy-subrc = 0.
                                  IF v_bin = 'X'.
                                    IF v_mhdhb > 0.
                                      it_lqua-vfdat = vekp-erdat + v_mhdhb.
                                    ELSE.
                                      it_lqua-vfdat =  vekp-erdat + 30.
                                    ENDIF.
                                  ELSE.
                                    it_lqua-vfdat+0(2) = '20'.
                                    it_lqua-vfdat+2(6) = vekp-inhalt+0(6).
                                    IF v_mhdhb > 0.
                                      it_lqua-vfdat = it_lqua-vfdat + v_mhdhb.
                                    ELSE.
                                      it_lqua-vfdat = it_lqua-vfdat + 30.
                                    ENDIF.
                                  ENDIF.
                                ENDIF.
                              ENDIF.
                            ENDIF.
                          ENDIF.
                        ENDIF.
                      ENDIF.

** End - Gokul 18MAY2004

                      UPDATE lqua FROM it_lqua.

                    ENDSELECT.

                  ENDIF.
                ELSE.
* 9. case                                                               9. case
************************************************************************
** To update ZTAFR_L_HEADER-QTY_PACK, ZTAFR_L_HEADER_WEIGHT,           *
** ZTAFR_L_HEADER-EXP_DATE and ZTAFR_L_HEADER_REFDOC_NO2 in case of    *
** single level packaging instruction  (LTAP-VNEST = ' ')              *
** Witte 19.11.2010                                                    *
************************************************************************

                  IF t_ltap_vb-lgnum = '305'
                    AND t_ltap_vb-vltyp = '901'
                    AND t_ltap_vb-nltyp = '100'
                    AND t_ltap_vb-lgort = '0004'
                    AND t_ltap_vb-vnest = ' '.
*                 IF ltap-vlenr GT '0'.
                    v_labelno =  t_ltap_vb-vlenr+10(10).
                    CONCATENATE c_prefix v_labelno INTO lheader-label_no.
                    SELECT SINGLE * FROM ztafr_l_header
                                    WHERE plant EQ t_ltap_vb-werks
                                     AND label_no EQ lheader-label_no.
                    IF sy-subrc EQ 0.
                      ztafr_l_header-qty_pack = t_ltap_vb-vistm.
                      ztafr_l_header-qty_unit  = t_ltap_vb-meins.
                      ztafr_l_header-weight  =  t_ltap_vb-brgew.
                      ztafr_l_header-exp_date  =  t_ltap_vb-vfdat.
                      ztafr_l_header-refdoc_no = t_ltap_vb-tanum.
                      ztafr_l_header-operation_no = t_ltap_vb-lgnum.
                      ztafr_l_header-refdoc_no2 = t_ltap_vb-wenum.

                      UPDATE ztafr_l_header.
                      CLEAR ztafr_l_header.
                    ENDIF.
                  ENDIF.                                    " 9. case
                ENDIF.                                      " 8. case
              ENDIF.                                        " 7. case
            ENDIF.                                          " 6. case
          ENDIF.                                            " 5. case
        ENDIF.                                              " 4. case
      ENDIF.                                                " 3. case
    ENDIF.                                                  " 2. case
  ENDIF.                                                    " 1. case

*** End of additions - Gokul 11MAR2004.

*******************************************************************************
* I.2. To determine, which type of labels to be printed                       *
*      To determine, the status of the lables ...                             *
*******************************************************************************

  SELECT * FROM ztafr_l_param INTO TABLE iztafr_l_param
               WHERE lgnum EQ t_ltap_vb-lgnum
               AND   vltyp EQ t_ltap_vb-vltyp
               AND   bwlvs EQ i_ltak_vb-bwlvs.
  IF sy-subrc NE 0.
    SELECT * FROM ztafr_l_param INTO TABLE iztafr_l_param
                 WHERE lgnum EQ t_ltap_vb-lgnum
                 AND   vltyp EQ '---'
                 AND   bwlvs EQ i_ltak_vb-bwlvs.
  ENDIF.

  LOOP AT iztafr_l_param.
* Mixed pallet
    IF iztafr_l_param-bwlvs = c_bwlvs4
       OR iztafr_l_param-bwlvs = c_bwlvs5.
      EXIT.
    ENDIF.
*Followng line modified - Gokul 04APR2004
    v_labelno =  t_ltap_vb-nlenr+10(10).
*    v_labelno = t_ltap_vb-vlenr+10(10).

    CONCATENATE c_prefix v_labelno INTO lheader-label_no.

* no label creation, but it will be printed
    IF iztafr_l_param-status IS INITIAL.
      EXIT.
    ENDIF.

* Label already exists
    SELECT SINGLE * FROM ztafr_l_header WHERE plant EQ
             t_ltap_vb-werks AND label_no EQ lheader-label_no.
    IF sy-subrc EQ 0.
      IF ztafr_l_header-qty_pack EQ t_ltap_vb-vsolm.
* If it is SFP , then no print
        IF ztafr_l_header-label_type = c_sfp.
          CLEAR f_print.
        ENDIF.
        EXIT.
      ELSE.
* else update the new qty in label table, allow print
* change only the qty field, pl.keep the old document numbers.
*           ztafr_l_header-REFDOC_NO  = T_LTAP_VB-TANUM.
*           ztafr_l_header-REFDOC_NO2  = T_LTAP_VB-WENUM.
        ztafr_l_header-qty_pack  = t_ltap_vb-vsolm.
        MODIFY ztafr_l_header FROM ztafr_l_header.
        EXIT.
      ENDIF.
    ENDIF.

    IF t_ltap_vb-vltyp EQ iztafr_l_param-vltyp
       AND i_ltak_vb-bwlvs EQ iztafr_l_param-bwlvs.

      c_label_type = iztafr_l_param-ltype. " 1.
      c_status = iztafr_l_param-status.                     "'01'.

      lheader-plant  = t_ltap_vb-werks.
      lheader-label_type = c_label_type.
      lheader-label_no  = lheader-label_no.
      lheader-label_matnr  = t_ltap_vb-matnr.
      WRITE sy-datum+0(4)  TO lheader-add_identi.
      lheader-label_desc  = t_ltap_vb-maktx.
      lheader-pack_matnr  = t_ltap_vb-vhilm.
      lheader-qty_pack  = t_ltap_vb-vsolm.
      lheader-qty_unit  = t_ltap_vb-meins.
      lheader-weight  = t_ltap_vb-brgew.
      lheader-exp_date  = t_ltap_vb-vfdat.
      lheader-refdoc_no  = t_ltap_vb-tanum.
      lheader-operation_no = t_ltap_vb-lgnum.
      lheader-refdoc_no2  = t_ltap_vb-wenum.
      lheader-createdby = sy-uname.
      lheader-createdon = sy-datum.
      lheader-vlabel_no = t_ltap_vb-zeugn.
      APPEND lheader.
    ENDIF.

    CLEAR lheader.
  ENDLOOP.
*      endif.                        "hier zu spät vgl 658
*    endif.                          "hier zu spät vgl 659
*  ENDIF.                            "hier zu spät vgl 660
ENDLOOP.

BREAK weberm.

*******************************************************************************
* II. create label in ZTAFR_L_HEADER                                          *
*******************************************************************************

LOOP AT lheader.
** create lable
  PERFORM create_lable IN PROGRAM zxltou01_sub USING lheader
                  CHANGING sysubrc.
** create status
  PERFORM create_status IN PROGRAM zxltou01_sub USING c_status
                   c_label_type l_counter
                   lheader.
  IF lheader-label_type = '4'.
    CALL FUNCTION 'ZTAFR_FIND_UC'
      EXPORTING
        werks         = lheader-plant
        label_type    = lheader-label_type
        label_no      = lheader-label_no
        add_identi    = lheader-add_identi
        update        = 'X'
      TABLES
        iztafr_l_item = iztafr_l_item.
**  This code get executed only in the time of Initial stock upload.
**  to create the UC labels which is there in the UM
**  IF SY-BATCH = 'X'.
*     LOOP AT IZTAFR_L_ITEM.
*          LHEADER-PLANT       =  IZTAFR_L_ITEM-PLANT.
*          LHEADER-LABEL_TYPE  =  IZTAFR_L_ITEM-I_LABEL_TYPE.
*          LHEADER-LABEL_NO    =  IZTAFR_L_ITEM-I_LABEL_NO.
*          LHEADER-ADD_IDENTI  =  IZTAFR_L_ITEM-I_ADD_IDENTI.
*          PERFORM CREATE_LABLE IN PROGRAM ZXLTOU01_SUB USING LHEADER.
*     ENDLOOP.
**  This code get executed only in the time of Initial stock upload.
**  to create the UC label status in the table ZTAFR_L_STATUS
**  IF SY-BATCH = 'X'.
*  LOOP AT IZTAFR_L_ITEM.
*      LHEADER-PLANT = IZTAFR_L_ITEM-PLANT.
*      LHEADER-LABEL_TYPE = IZTAFR_L_ITEM-I_LABEL_TYPE.
*      LHEADER-LABEL_NO = IZTAFR_L_ITEM-I_LABEL_NO.
*      LHEADER-ADD_IDENTI = IZTAFR_L_ITEM-I_ADD_IDENTI.
*** create status
*     PERFORM CREATE_STATUS IN PROGRAM ZXLTOU01_SUB USING C_STATUS
*                              IZTAFR_L_ITEM-I_LABEL_TYPE L_COUNTER
*                              LHEADER.
*  ENDLOOP.
*  ENDIF.
  ENDIF.


*  ENDIF.
ENDLOOP.
CLEAR lheader. REFRESH lheader.

BREAK weberm.

*******************************************************************************
* III. Process printout depending values in param table                         *
*******************************************************************************
*Begin of Insertion D02K941540
DATA: label_data TYPE vwahn,
      ls_tabix   TYPE sy-tabix.
IF sy-tcode <> 'LT15'.
  SELECT SINGLE *
    FROM likp
    INTO @DATA(ls_likp)
    WHERE vbeln = @t_ltap_vb-vbeln.
  DATA: gt_vekp  TYPE TABLE OF vekp,
        gt_vekp1 TYPE TABLE OF vekp,
        lt_small TYPE TABLE OF vekp.
  SELECT SINGLE *
    FROM zmaintain_print
    INTO @DATA(ls_print1)
    WHERE parnr = @ls_likp-kunnr
    AND parvw = 'WE'
    AND werks = @t_ltap_vb-werks
    AND object1 = 'TO_CREATE'
    AND deletion = ' '.
  IF sy-subrc = 0 .
    CLEAR f_print.
*    SELECT SINGLE *
*      FROM tsp03d
*      INTO @DATA(ls_tsp03d)
*      WHERE name = @ls_print1-object2.

    SELECT *
  FROM zsd_to_hu_print
  INTO TABLE @DATA(lt_hu)
  WHERE lgnum = @i_ltak_vb-lgnum
  AND tanum = @i_ltak_vb-tanum.
*  IF sy-subrc = 0.
    IF t_ltap_vb[] IS NOT INITIAL.
      SELECT *
        FROM vekp
        INTO TABLE @DATA(lt_vekp)
        FOR ALL ENTRIES IN @t_ltap_vb
        WHERE exidv = @t_ltap_vb-vlenr.
    ENDIF.
    IF lt_vekp IS NOT INITIAL.
      LOOP AT t_ltap_vb INTO DATA(ltap1).
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
        APPEND ls_vekp1 TO lt_small.
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
*    LOOP AT lt_vekp ASSIGNING FIELD-SYMBOL(<fs>).
*      <fs>-vbeln_gen = t_ltap_vb-vbeln.
*    ENDLOOP.
*    LOOP AT gt_vekp ASSIGNING <fs>.
*      <fs>-vbeln_gen = t_ltap_vb-vbeln.
*    ENDLOOP.
    CLEAR gt_vekp1.
    IF ls_print1-value1 = 'MASTER'.
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
*    gt_vekp1 = gt_vekp.
    ELSE.
*    gt_vekp1 = lt_vekp.
      LOOP AT lt_small INTO gs_vekp.
        ls_hu = VALUE #( lt_hu[ lgnum = i_ltak_vb-lgnum tanum = i_ltak_vb-tanum exidv = gs_vekp-exidv ] OPTIONAL ).
        IF ls_hu IS INITIAL.
          APPEND gs_vekp TO gt_vekp1.
        ENDIF.
        CLEAR: ls_hu, gs_vekp.
      ENDLOOP.
    ENDIF.
    DELETE ADJACENT DUPLICATES FROM gt_vekp1 COMPARING ALL FIELDS.
    LOOP AT gt_vekp1 INTO DATA(ls_vekp).
      DATA(ls_b904) = VALUE #( lt_b904[ vhilm = ls_vekp-vhilm ] OPTIONAL ).
      IF ls_b904 IS NOT INITIAL.
        IF ls_b904-kschl <> 'ZSRB' AND ls_b904-kschl <> 'ZSRNB'.
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
            tnapr-sform = ls_tnapr-sform.
            nast-dimme = 'X'.
            nast-kschl = ls_b904-kschl.
            nast-kappl = 'V6'.
            PERFORM (ls_tnapr-ronam) IN PROGRAM (ls_tnapr-pgnam) USING
            ' ' ''. "ls_vekp.
            CLEAR ls_hu.
            ls_hu-lgnum = i_ltak_vb-lgnum.
            ls_hu-tanum = i_ltak_vb-tanum.
            ls_hu-exidv = ls_vekp-exidv.
            ls_hu-object1 = 'TO_CREATE'.
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
            tnapr-sform = ls_tnapr-sform.
            nast-dimme = 'X'.
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
              ls_hu-object1 = 'TO_CREATE'.
              ls_hu-created_by = sy-uname.
              ls_hu-created_on = sy-datum.
              ls_hu-created_time = sy-uzeit.
              MODIFY zsd_to_hu_print FROM ls_hu.
              CLEAR gs_vekp.
            ENDLOOP.
            EXIT.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDLOOP.
    CALL FUNCTION 'Z_HU_INTERNAL_LABELS'
      EXPORTING
        ltak = i_ltak_vb
      TABLES
        ltap = t_ltap_vb.

  ENDIF.
ENDIF.
CLEAR: gt_vekp, lt_vekp, lt_vepo, gt_vekp1, lt_b904, lt_hu, lt_small.
*End of Insertion D02K941540

IF f_print = 'X'.
  IF  i_ltak_vb-bwlvs =  iztafr_l_param-bwlvs.
    IF iztafr_l_param-ltype = c_rm.
*      if ltap-werks = '3335'
*      Begin change M. Weber 20061009 "new" plant Rethel 3364
*      same printing as Plant 3335 Soratech
*      IF ( ltap-werks = '3335' OR
*           ltap-werks = '3364' ) AND
*           ltap-matnr+0(1) = '7'.
***********
*      IF ( ltap-werks = '3335' AND
*              ltap-matnr+0(1) = '7' ) OR
*         ( ltap-werks = '3364' AND
*              ltap-matnr+0(1) = '5' ) OR
*         ( ltap-werks = '3364' AND
*              ltap-matnr+0(1) = '7' ) .
*      End change M. Weber
*      Begin Change MB 20090129
      IF ( ltap-werks = '3335' AND
             ltap-matnr+0(1) = '7' ) OR
          ( ltap-werks = '3364' AND
             ltap-matnr+0(1) = '5' ) OR
          ( ltap-werks = '3364' AND
             ltap-matnr+0(1) = '7' ) OR
          ( ltap-werks = '3405' AND
             ltap-matnr+0(1) = '5' ) OR
          ( ltap-werks = '3405' AND
             ltap-matnr+0(1) = '7' ) .
*      End Change MB 20090129


        CALL FUNCTION 'FUNCTION_EXISTS'
          EXPORTING
            funcname           = 'Z_HU_MX'
          EXCEPTIONS
            function_not_exist = 1
            OTHERS             = 2.

        IF sy-subrc = 0.
          CALL FUNCTION 'Z_HU_MX'
            EXPORTING
              ltak = i_ltak_vb
            TABLES
              ltap = t_ltap_vb.

        ENDIF.
*      Begin change M. Weber 20061023
*      perform for insert the table ztafr_l_status
*      Begin Change MB 20090129
*       IF ltap-werks = '3364'.
        IF ltap-werks = '3364' OR
           ltap-werks = '3405' .
*      End Change MB 20090129


          PERFORM rethel_exit_mixing_production
                                USING ltap
                                iztafr_l_param-ltype.

        ENDIF.
*      End change M. Weber
      ELSE.
        CALL FUNCTION 'FUNCTION_EXISTS'
          EXPORTING
            funcname           = 'Z_HU_RM'
          EXCEPTIONS
            function_not_exist = 1
            OTHERS             = 2.

        IF sy-subrc = 0.
          CALL FUNCTION 'Z_HU_RM'
            EXPORTING
              ltak = i_ltak_vb
            TABLES
              ltap = t_ltap_vb.

        ENDIF.
      ENDIF.
    ELSEIF  iztafr_l_param-ltype = c_um.
* >>> Martin Wagner - Feb 2008
      CLEAR: ls_ltak_ctrl.
      MOVE-CORRESPONDING i_ltak_vb TO ls_ltak_ctrl.
      CALL FUNCTION 'Z_SD_LABCTRL_GET_VERSION'
        EXPORTING
          ltak       = ls_ltak_ctrl
          levent     = '01'
        IMPORTING
          logic_vers = lv_logic_version.

      CASE lv_logic_version.
        WHEN '01'.
          CALL FUNCTION 'Z_HU_UM'
            EXPORTING
              ltak = i_ltak_vb
            TABLES
              ltap = t_ltap_vb.
        WHEN '02'.
          CALL FUNCTION 'Z_HU_UM_02'
            EXPORTING
              ltak = i_ltak_vb
            TABLES
              ltap = t_ltap_vb.
        WHEN '03'.
          CALL FUNCTION 'Z_HU_UM_03'
            EXPORTING
              ltak = i_ltak_vb
            TABLES
              ltap = t_ltap_vb.
        WHEN '04'.
      ENDCASE.
    ENDIF.
  ENDIF.
ENDIF.

*******************************************************************************
* IV. Delete UM and (or) UC                                                   *
*******************************************************************************

CLEAR v_labelno.


IF i_ltak_vb-bwlvs = c_bwlvs_del.
  LOOP AT t_ltap_vb.
    IF t_ltap_vb-homve IS INITIAL.
      v_labelno =  t_ltap_vb-vlenr+10(10).
      lheader-label_type = c_um.
      CONCATENATE c_prefix v_labelno INTO lheader-label_no.
      lheader-label_no  = lheader-label_no.
      WRITE sy-datum+0(4)  TO lheader-add_identi.
      lheader-plant = t_ltap_vb-werks.
*       SELECT SINGLE * FROM VEKP WHERE EXIDV EQ LHEADER-LABEL_NO.
*       IF VEKP-STATUS EQ '0060'.
      APPEND lheader.
*       ENDIF.
      CLEAR lheader.
      EXIT.
    ENDIF.
  ENDLOOP.

*     Set delete flag for the UM
  LOOP AT lheader.
*    * create status
    SELECT MAX( counter ) INTO l_counter FROM ztafr_l_status
       WHERE plant EQ lheader-plant
       AND label_type EQ lheader-label_type
       AND label_no EQ lheader-label_no
       AND add_identi EQ lheader-add_identi.

    l_counter = l_counter + 1.
    PERFORM create_status IN PROGRAM zxltou01_sub USING c_status_del
                                   c_um l_counter
                                   lheader.
*     Delete the UC link with the UM.
    CALL FUNCTION 'ZTAFR_FIND_UC'
      EXPORTING
        werks      = lheader-plant
        label_type = lheader-label_type
        label_no   = lheader-label_no
        add_identi = lheader-add_identi
        update     = 'D'.
*          TABLES
*            IZTAFR_L_ITEM       = .
  ENDLOOP.
  CLEAR : lheader, ztafr_l_status.
  REFRESH : lheader.
ENDIF.

*******************************************************************************
* V. Update status to '03' for material staging                               *
*******************************************************************************

** if PQUIT = 'X' ( Immediate Confirmation ).

DATA: d_label_type    TYPE c,
      d_add_identi(4) TYPE n,
      d_vlenr(12)     TYPE n,
      d_counter       TYPE i,
      d_seq           LIKE ztafr_status-status_seq.

LOOP AT t_ltap_vb WHERE pquit = 'X'.


  IF NOT ( t_ltap_vb-lgnum EQ '305' OR
   t_ltap_vb-lgnum EQ '335' OR
   t_ltap_vb-lgnum EQ '340' ).
    CONTINUE.
  ENDIF.

  IF NOT ( t_ltap_vb-nltyp EQ '100' OR
   t_ltap_vb-nltyp EQ '101' OR
   t_ltap_vb-nltyp EQ '102' ).
    CONTINUE.
  ENDIF.
  PACK t_ltap_vb-vlenr TO d_vlenr.

* SELECT KEY FIELDS FROM ZTAFR_L_HEADER.
  SELECT SINGLE label_type add_identi
        INTO (d_label_type , d_add_identi)
        FROM  ztafr_l_header
       WHERE plant    = t_ltap_vb-werks
         AND label_no = d_vlenr.
  IF sy-subrc = 0.
    SELECT SINGLE * FROM ztafr_l_status
               WHERE plant      = t_ltap_vb-werks
                 AND label_type = d_label_type
                 AND label_no   = d_vlenr
                 AND add_identi = d_add_identi
                 AND status     = '01'.
    IF  sy-subrc EQ 0.
      SELECT SINGLE * FROM ztafr_l_status
                    WHERE plant      = t_ltap_vb-werks
                      AND label_type = d_label_type
                      AND label_no   = d_vlenr
                      AND add_identi = d_add_identi
                      AND status     = '03'.
      IF sy-subrc NE 0.
*DETERMINE COUNTER
        SELECT SINGLE MAX( counter ) INTO d_counter FROM
                                    ztafr_l_status WHERE
                                    plant = t_ltap_vb-werks AND
                                    label_type = d_label_type AND
                                     label_no = d_vlenr AND
                                     add_identi = d_add_identi.
* SELECT STATUS SEQUANCE FROM TABLE ZTAFR_STATUS
        SELECT SINGLE status_seq INTO d_seq
                                 FROM ztafr_status
                                WHERE label_type EQ d_label_type
                                  AND label_status EQ '03'.
*INSERTED THE RECORD INTO TABLE ZTAFR_L_STATUS
        ztafr_l_status-plant      = ztafr_l_status-plant.
        ztafr_l_status-label_type = ztafr_l_status-label_type.
        ztafr_l_status-label_no   = ztafr_l_status-label_no .
        ztafr_l_status-add_identi = ztafr_l_status-add_identi.
        ztafr_l_status-status     = '03'.
        ztafr_l_status-counter    = d_counter + 1.
        ztafr_l_status-status_seq = d_seq.
        ztafr_l_status-created_on = ztafr_l_status-created_on.
        ztafr_l_status-created_at = ztafr_l_status-created_at.
        ztafr_l_status-created_by = ztafr_l_status-created_by.
        ztafr_l_status-updated_by = ztafr_l_status-updated_by.
        INSERT ztafr_l_status.
        CLEAR ztafr_l_status.
      ENDIF.
    ENDIF.
  ENDIF.
ENDLOOP.

*******************************************************************************
* VI. WM-0038 TO Confirmation - Gokul 05APR2004                               *
*******************************************************************************

IF sy-tcode = 'LT10'.    " or SY-TCODE = 'LT03'.


  DATA: i_ltap LIKE STANDARD TABLE OF ltap_conf WITH DEFAULT KEY
                              WITH HEADER LINE.

  DATA: i_hu LIKE STANDARD TABLE OF ltap_conf_hu WITH DEFAULT KEY
                            WITH HEADER LINE.

  DATA: t1 LIKE sy-uzeit.

  LOOP AT t_ltap_vb.

** Check for field FHUTA in LTAP NE 0 - As requested by Stelios
** Gokul 22SEP2004.
    IF t_ltap_vb-fhuta EQ 0.
      CONTINUE.
    ENDIF.


* Check if Auto Confirm Flag is set in table ZTAFR_L_PLANT
    SELECT SINGLE * FROM ztafr_l_plant WHERE plant = t_ltap_vb-werks
                                         AND auto_confrim_hu = 'X'.
    IF sy-subrc = 0.

      SELECT SINGLE * FROM t333 WHERE
                    lgnum = t_ltap_vb-lgnum
                AND bwlvs = t_ltap_vb-bwlvs
                AND squit = 'X'
                AND vquit = 'X'.

      IF sy-subrc = 0.
        REFRESH i_ltap. CLEAR i_ltap.

        i_ltap-tanum = t_ltap_vb-tanum.
        i_ltap-tapos = t_ltap_vb-tapos.
        i_ltap-squit = 'X'.

        APPEND i_ltap. CLEAR i_ltap.

        REFRESH i_hu. CLEAR i_hu.

        i_hu-tanum  = t_ltap_vb-tanum.
        i_hu-tapos  = t_ltap_vb-tapos.
        i_hu-vonhu  = t_ltap_vb-vlenr.
        i_hu-nachu  = t_ltap_vb-vlenr.
        i_hu-menga  = t_ltap_vb-nsola.
        i_hu-altme  = t_ltap_vb-altme.
        i_hu-huent  = 'X'.

        APPEND i_hu. CLEAR i_hu.

        CALL FUNCTION 'L_TO_CONFIRM' IN BACKGROUND TASK
          EXPORTING
            i_lgnum        = t_ltap_vb-lgnum
            i_tanum        = t_ltap_vb-tanum
          TABLES
            t_ltap_conf    = i_ltap
            t_ltap_conf_hu = i_hu.

*        CLEAR t1.
*
*        t1 = sy-uzeit + 15.
*
*        CALL FUNCTION 'START_OF_BACKGROUNDTASK'
*             EXPORTING
*                  startdate = sy-datum
*                  starttime = t1
*             EXCEPTIONS
*                  OTHERS    = 1.

      ENDIF.
    ENDIF.
  ENDLOOP.

ENDIF.

* Bauer MHP 15.06.2010
* Für den Prozess Vulcanization WM Hradek muss die Befüllung der Tabelle
* ZWM_RF_SU_LABEL erst nach dem Erstellen des Transportauftrags er-
* folgen, da erst dann die Lagereinheitennummer zur Verfügung steht.

IF i_ltak_vb-lgnum = '420' AND ( i_ltak_vb-bwlvs = '903'
                               OR i_ltak_vb-bwlvs = '312' )
                           AND i_ltak_vb-trart = 'E'.

  TABLES: zwm_rf_su_label.
  DATA : wa_su_label TYPE zwm_rf_su_label.


  SELECT SINGLE * FROM zwm_rf_su_label
                  INTO zwm_rf_su_label
                  WHERE werks EQ t_ltap_vb-werks
                    AND lenum EQ t_ltap_vb-nlenr.
  IF sy-subrc = '0'.

    zwm_rf_su_label-menge  = t_ltap_vb-nistm.
    zwm_rf_su_label-menge  = t_ltap_vb-nistm.
    zwm_rf_su_label-datwe  = t_ltap_vb-wdatu + 1.
    zwm_rf_su_label-datve  = t_ltap_vb-vfdat.
    zwm_rf_su_label-lot    = i_ltak_vb-mblnr.
    zwm_rf_su_label-erdat  = t_ltap_vb-wdatu.
    MODIFY zwm_rf_su_label.

  ELSE.

    wa_su_label-lenum  = t_ltap_vb-nlenr.
    wa_su_label-werks  = t_ltap_vb-werks.
    wa_su_label-matnr  = t_ltap_vb-matnr.
    wa_su_label-menge  = t_ltap_vb-nistm.
    wa_su_label-meins  = t_ltap_vb-meins.
    wa_su_label-datwe  = t_ltap_vb-wdatu + 1.
    wa_su_label-datve  = t_ltap_vb-vfdat.
    wa_su_label-atrag  = ''.
    wa_su_label-charge = ''.
    wa_su_label-packag = ''.
    wa_su_label-lot    = i_ltak_vb-mblnr.
    wa_su_label-ernam  = i_ltak_vb-bname.
    wa_su_label-erdat  = t_ltap_vb-wdatu.
    wa_su_label-eruhr  = i_ltak_vb-bzeit.

    INSERT INTO zwm_rf_su_label VALUES wa_su_label.

  ENDIF.

ENDIF.

*" Begin of Insertion D02K938441
*DATA: ls_nast  TYPE nast,
*      ls_print TYPE zwm_to_print,
*      lv_code  TYPE i.
*
*ls_nast-objky = i_ltak_vb-tanum.
*ls_nast-ldest      = t_ltap_vb-ldest.
*ls_nast-dsuf2      = '    '.
*
*SELECT SINGLE *
*       FROM zwm_to_print
*       INTO ls_print
*       WHERE lgnum = i_ltak_vb-lgnum
*       AND drukz = i_ltak_vb-drukz.
*IF sy-subrc = 0.
*  PERFORM (ls_print-ronam) IN PROGRAM (ls_print-pgnam) USING
*        ls_nast ' ' lv_code i_ltak_vb-lgnum.
*ENDIF.
" End of Insertion D02K938441
