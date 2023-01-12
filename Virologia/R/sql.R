conf <- "SELECT
  dbo.Conferimenti.Numero As nconf,
  dbo.Anag_Registri.Descrizione As settore,
  dbo_Anag_Finalita_Confer.Descrizione As finalita,
  dbo.Anag_Motivi_Prelievo.Descrizione As motivo_prel,
  dbo_Anag_Reparti_ConfAcc.Descrizione AS str_acc,
  dbo_Anag_Reparti_ConfProp.Descrizione As str_propr,
  dbo_Anag_Bool_Insolvenza.Descrizione2 As insolvente,
  dbo.Conferimenti.Temperatura_Campione_Sentinella As temp_campsent,
  dbo.Conferimenti.Data_Prelievo As dtprel,
  dbo.Conferimenti.Data As dtconf,
  dbo.Conferimenti.Data_Accettazione As dtreg,
  dbo.Conferimenti_Scostamenti.Scostamento As scostamento,
  dbo.Anag_Specie.Descrizione As specie,
  dbo.Anag_Materiali.Descrizione As materiale,
  dbo.Anag_Matrici.Descrizione As matrice,
  dbo.Anag_Origine.Descrizione As origine,
  dbo_Anag_Referenti_Prop.Ragione_Sociale As proprietario,
  dbo.Anag_Referenti.Ragione_Sociale As conferente,
  dbo_Anag_Referenti_Veter.Ragione_Sociale As veterinario,
  dbo_Anag_Referenti_DitteProd.Ragione_Sociale As ditta_prod,
  dbo_Anag_Referenti_DestFatt.Ragione_Sociale As dest_fatt,
  dbo_Anag_Referenti_DestRdP.Ragione_Sociale As dest_rdp,
  dbo.Conferimenti.Allevix_Proprietario As codaz,
  dbo.Anag_Regioni.Descrizione As regione,
  dbo.Anag_Comuni.Provincia As provincia,
  dbo.Anag_Comuni.Descrizione As comune, 
  dbo.Anag_Asl.Descrizione As ASL,
  dbo.Anag_Tipo_Prel.Descrizione As tipo_prelievo,
  dbo.Anag_TipoConf.Descrizione As pagamento,
  dbo.Conferimenti.NrCampioni,
  dbo.Esami_Aggregati.Data_Invio,
  dbo.Esami_Aggregati.Data_Carico,
  convert (SMALLDATETIME, dbo.Conferimenti.Data_Primo_RDP_Completo_Firmato) As dtprimordp,
  dbo.RDP_Date_Emissione.Istanza_RDP,
  dbo.Conferimenti.Riferimenti As verbale,
  dbo.RDP_Date_Emissione.SISI_Numero_RDP,
  dbo.RDP_Date_Emissione.SISI_Data_Firma_RDP
FROM
{ oj dbo.Anag_Registri INNER JOIN dbo.Conferimenti ON ( dbo.Conferimenti.Registro=dbo.Anag_Registri.Codice )
   INNER JOIN dbo.Anag_Comuni ON ( dbo.Anag_Comuni.Codice=dbo.Conferimenti.Luogo_Prelievo )
   LEFT OUTER JOIN dbo.Anag_Regioni ON ( dbo.Anag_Regioni.Codice=dbo.Anag_Comuni.Regione )
   INNER JOIN dbo.Anag_Referenti ON ( dbo.Conferimenti.Conferente=dbo.Anag_Referenti.Codice )
   LEFT OUTER JOIN dbo.Anag_Matrici ON ( dbo.Conferimenti.Matrice=dbo.Anag_Matrici.Codice )
   LEFT OUTER JOIN dbo.Esami_Aggregati ON ( dbo.Conferimenti.Anno=dbo.Esami_Aggregati.Anno_Conferimento and dbo.Conferimenti.Numero=dbo.Esami_Aggregati.Numero_Conferimento )
   INNER JOIN dbo.Anag_Tipo_Prel ON ( dbo.Conferimenti.Tipo_Prelievo=dbo.Anag_Tipo_Prel.Codice )
   INNER JOIN dbo.Anag_Referenti  dbo_Anag_Referenti_DestRdP ON ( dbo.Conferimenti.Dest_Rapporto_Prova=dbo_Anag_Referenti_DestRdP.Codice )
   INNER JOIN dbo.Laboratori_Reparto  dbo_Laboratori_Reparto_ConfProp ON ( dbo.Conferimenti.RepLab=dbo_Laboratori_Reparto_ConfProp.Chiave )
   INNER JOIN dbo.Anag_Reparti  dbo_Anag_Reparti_ConfProp ON ( dbo_Laboratori_Reparto_ConfProp.Reparto=dbo_Anag_Reparti_ConfProp.Codice )
   INNER JOIN dbo.Laboratori_Reparto  dbo_Laboratori_Reparto_ConfAcc ON ( dbo.Conferimenti.RepLab_Conferente=dbo_Laboratori_Reparto_ConfAcc.Chiave )
   INNER JOIN dbo.Anag_Reparti  dbo_Anag_Reparti_ConfAcc ON ( dbo_Laboratori_Reparto_ConfAcc.Reparto=dbo_Anag_Reparti_ConfAcc.Codice )
   LEFT OUTER JOIN dbo.Anag_Referenti  dbo_Anag_Referenti_Prop ON ( dbo_Anag_Referenti_Prop.Codice=dbo.Conferimenti.Proprietario )
   LEFT OUTER JOIN dbo.Anag_Referenti  dbo_Anag_Referenti_Veter ON ( dbo.Conferimenti.Veterinario=dbo_Anag_Referenti_Veter.Codice )
   LEFT OUTER JOIN dbo.Anag_Referenti  dbo_Anag_Referenti_DitteProd ON ( dbo.Conferimenti.Ditta_Prod=dbo_Anag_Referenti_DitteProd.Codice )
   LEFT OUTER JOIN dbo.Anag_Referenti  dbo_Anag_Referenti_DestFatt ON ( dbo_Anag_Referenti_DestFatt.Codice=dbo.Conferimenti.Dest_Fattura )
   INNER JOIN dbo.Anag_TipoConf ON ( dbo.Anag_TipoConf.Codice=dbo.Conferimenti.Tipo )
   INNER JOIN dbo.Anag_Origine ON ( dbo.Anag_Origine.Codice=dbo.Conferimenti.Origine )
   LEFT OUTER JOIN dbo.Anag_Materiali ON ( dbo.Anag_Materiali.Codice=dbo.Conferimenti.Codice_Materiale )
   LEFT OUTER JOIN dbo.Anag_Specie ON ( dbo.Anag_Specie.Codice=dbo.Conferimenti.Codice_Specie )
   LEFT OUTER JOIN dbo.Anag_Asl ON ( dbo.Anag_Asl.codice=dbo.Conferimenti.Cod_Asl )
   INNER JOIN dbo.Conferimenti_Finalita ON ( dbo.Conferimenti.Anno=dbo.Conferimenti_Finalita.Anno and dbo.Conferimenti.Numero=dbo.Conferimenti_Finalita.Numero )
   INNER JOIN dbo.Anag_Finalita  dbo_Anag_Finalita_Confer ON ( dbo.Conferimenti_Finalita.Finalita=dbo_Anag_Finalita_Confer.Codice )
   LEFT OUTER JOIN dbo.RDP_Date_Emissione ON ( dbo.RDP_Date_Emissione.Anno=dbo.Conferimenti.Anno and dbo.RDP_Date_Emissione.Numero=dbo.Conferimenti.Numero )
   LEFT OUTER JOIN dbo.Conferimenti_Motivi_Prelievo ON ( dbo.Conferimenti_Motivi_Prelievo.Anno_Conferimento=dbo.Conferimenti.Anno and dbo.Conferimenti_Motivi_Prelievo.Numero_Conferimento=dbo.Conferimenti.Numero )
   LEFT OUTER JOIN dbo.Anag_Motivi_Prelievo ON ( dbo.Anag_Motivi_Prelievo.Codice=dbo.Conferimenti_Motivi_Prelievo.Motivo_Prelievo )
   INNER JOIN dbo.Anag_Boolean  dbo_Anag_Bool_Insolvenza ON ( dbo_Anag_Bool_Insolvenza.Codice=dbo.Conferimenti.Accettato_Con_Insolvenza )
   LEFT OUTER JOIN dbo.Conferimenti_Scostamenti ON ( dbo.Conferimenti.Anno=dbo.Conferimenti_Scostamenti.Anno_Conferimento and dbo.Conferimenti.Numero=dbo.Conferimenti_Scostamenti.Numero_Conferimento )
  }
WHERE
  
  {fn year(dbo.Conferimenti.Data)}  >=  2021
  AND dbo_Anag_Reparti_ConfAcc.Descrizione  IN  ('Reparto Virologia', 'Reparto Virologia (ME)', 'Reparto Virologia - Laboratorio Proteomica')
"


proveIN <- "SELECT        Conferimenti.Numero AS nconf, { fn YEAR(Conferimenti.Data) } AS anno, Anag_Reparti.Descrizione AS str_analisi, Anag_Laboratori.Descrizione AS lab_analisi, Anag_Metodi_di_Prova.Descrizione AS mp, 
                         Anag_Tecniche.Descrizione AS tecnica, Anag_Prove.Descrizione AS prova, Indice_Campioni_Esaminati.Numero_Campione AS numero_del_campione, Esami_Aggregati.Data_Inizio_Analisi AS dtinizio, 
                         Esami_Aggregati.Data_Fine_Analisi AS dtfine, Nomenclatore_Range.Valore AS valore, Nomenclatore_Range.ModEspr AS modexpr, Nomenclatore_Range.ModEspr2 AS modexpr2, Risultati_Analisi.Segno AS segno, 
                         Risultati_Analisi.Valore AS valore2, dbo_Anag_Tipo_Dett_Diagnostica.Descrizione AS dett1, dbo_Anag_Dettagli_Diagnostica.Dettaglio AS dett2, Risultati_Analisi_Dettagli.Testo_Dettaglio AS testodett, Risultati_Analisi.Esito, 
                         Anag_Esiti.Descrizione As esiti
FROM            Anag_Esiti INNER JOIN
                         Risultati_Analisi ON Anag_Esiti.Codice = Risultati_Analisi.Esito RIGHT OUTER JOIN
                         Anag_Reparti RIGHT OUTER JOIN
                         Laboratori_Reparto ON Laboratori_Reparto.Reparto = Anag_Reparti.Codice RIGHT OUTER JOIN
                         Esami_Aggregati ON Esami_Aggregati.RepLab_analisi = Laboratori_Reparto.Chiave RIGHT OUTER JOIN
                         Conferimenti ON Conferimenti.Anno = Esami_Aggregati.Anno_Conferimento AND Conferimenti.Numero = Esami_Aggregati.Numero_Conferimento LEFT OUTER JOIN
                         Conferimenti_Movimentazione ON Conferimenti.Anno = Conferimenti_Movimentazione.Anno_Conferimento AND Conferimenti_Movimentazione.Numero_Conferimento = Conferimenti.Numero LEFT OUTER JOIN
                         Laboratori_Reparto AS dbo_Laboratori_Reparto_Mitt ON Conferimenti_Movimentazione.RepLab_che_invia = dbo_Laboratori_Reparto_Mitt.Chiave LEFT OUTER JOIN
                         Anag_Reparti AS dbo_Anag_Reparti_Mitt ON dbo_Laboratori_Reparto_Mitt.Reparto = dbo_Anag_Reparti_Mitt.Codice LEFT OUTER JOIN
                         Nomenclatore_MP ON Esami_Aggregati.Nomenclatore = Nomenclatore_MP.Codice LEFT OUTER JOIN
                         Anag_Metodi_di_Prova ON Nomenclatore_MP.MP = Anag_Metodi_di_Prova.Codice LEFT OUTER JOIN
                         Nomenclatore_Settori ON Nomenclatore_MP.Nomenclatore_Settore = Nomenclatore_Settori.Codice LEFT OUTER JOIN
                         Nomenclatore ON Nomenclatore_Settori.Codice_Nomenclatore = Nomenclatore.Chiave LEFT OUTER JOIN
                         Anag_Prove ON Nomenclatore.Codice_Prova = Anag_Prove.Codice LEFT OUTER JOIN
                         Anag_Tecniche ON Nomenclatore.Codice_Tecnica = Anag_Tecniche.Codice INNER JOIN
                         Indice_Campioni_Esaminati ON Esami_Aggregati.Anno_Conferimento = Indice_Campioni_Esaminati.Anno_Conferimento AND Esami_Aggregati.Numero_Conferimento = Indice_Campioni_Esaminati.Numero_Conferimento AND 
                         Esami_Aggregati.Codice = Indice_Campioni_Esaminati.Codice ON Risultati_Analisi.Anno_Conferimento = Indice_Campioni_Esaminati.Anno_Conferimento AND 
                         Risultati_Analisi.Numero_Conferimento = Indice_Campioni_Esaminati.Numero_Conferimento AND Risultati_Analisi.Codice = Indice_Campioni_Esaminati.Codice AND 
                         Risultati_Analisi.Numero_Campione = Indice_Campioni_Esaminati.Numero_Campione LEFT OUTER JOIN
                         Nomenclatore_Range ON Risultati_Analisi.Range = Nomenclatore_Range.Codice LEFT OUTER JOIN
                         Risultati_Analisi_Dettagli ON Indice_Campioni_Esaminati.Anno_Conferimento = Risultati_Analisi_Dettagli.Anno_Conferimento AND 
                         Indice_Campioni_Esaminati.Numero_Conferimento = Risultati_Analisi_Dettagli.Numero_Conferimento AND Indice_Campioni_Esaminati.Numero_Campione = Risultati_Analisi_Dettagli.Numero_Campione AND 
                         Indice_Campioni_Esaminati.Codice = Risultati_Analisi_Dettagli.Codice_Programmazione LEFT OUTER JOIN
                         Anag_Dettagli AS dbo_Anag_Dettagli_Diagnostica ON Risultati_Analisi_Dettagli.Codice_Dettaglio = dbo_Anag_Dettagli_Diagnostica.Codice LEFT OUTER JOIN
                         Anag_Tipo_Dett AS dbo_Anag_Tipo_Dett_Diagnostica ON Risultati_Analisi_Dettagli.Tipo_Dettaglio = dbo_Anag_Tipo_Dett_Diagnostica.Codice LEFT OUTER JOIN
                         Anag_Laboratori ON Laboratori_Reparto.Laboratorio = Anag_Laboratori.Codice
WHERE        (Esami_Aggregati.Esame_Altro_Ente = 0) AND (Esami_Aggregati.Esame_Altro_Ente = 0) AND ({ fn YEAR(Conferimenti.Data) } >= 2021) AND (Anag_Reparti.Descrizione  IN  ('Reparto Virologia', 'Reparto Virologia (ME)', 'Reparto Virologia - Laboratorio Proteomica'))
"


proveOUT <- "SELECT        Conferimenti.Numero AS nconf, { fn YEAR(Conferimenti.Data) } AS anno, Anag_Reparti.Descrizione AS str_analisi, Anag_Laboratori.Descrizione AS lab_analisi, Anag_Metodi_di_Prova.Descrizione AS mp, 
                         Anag_Tecniche.Descrizione AS tecnica, Anag_Prove.Descrizione AS prova, Indice_Campioni_Esaminati.Numero_Campione AS numero_del_campione, Esami_Aggregati.Data_Invio, Esami_Aggregati.Data_Carico, 
                         Esami_Aggregati.Data_Inizio_Analisi AS dtinizio, Esami_Aggregati.Data_Fine_Analisi AS dtfine, Nomenclatore_Range.Valore AS valore, Nomenclatore_Range.ModEspr AS modexpr, 
                         Nomenclatore_Range.ModEspr2 AS modexpr2, Risultati_Analisi.Segno AS segno, Risultati_Analisi.Valore AS valore2, dbo_Anag_Tipo_Dett_Diagnostica.Descrizione AS dett1, dbo_Anag_Dettagli_Diagnostica.Dettaglio AS dett2, 
                         Risultati_Analisi_Dettagli.Testo_Dettaglio AS testodett, Risultati_Analisi.Esito, Anag_Esiti.Descrizione As esiti
FROM            Anag_Esiti INNER JOIN
                         Risultati_Analisi ON Anag_Esiti.Codice = Risultati_Analisi.Esito RIGHT OUTER JOIN
                         Anag_Reparti AS dbo_Anag_Reparti_ConfAcc INNER JOIN
                         Laboratori_Reparto AS dbo_Laboratori_Reparto_ConfAcc ON dbo_Laboratori_Reparto_ConfAcc.Reparto = dbo_Anag_Reparti_ConfAcc.Codice INNER JOIN
                         Conferimenti ON Conferimenti.RepLab_Conferente = dbo_Laboratori_Reparto_ConfAcc.Chiave LEFT OUTER JOIN
                         Esami_Aggregati ON Conferimenti.Anno = Esami_Aggregati.Anno_Conferimento AND Conferimenti.Numero = Esami_Aggregati.Numero_Conferimento LEFT OUTER JOIN
                         Nomenclatore_MP ON Esami_Aggregati.Nomenclatore = Nomenclatore_MP.Codice LEFT OUTER JOIN
                         Anag_Metodi_di_Prova ON Nomenclatore_MP.MP = Anag_Metodi_di_Prova.Codice LEFT OUTER JOIN
                         Nomenclatore_Settori ON Nomenclatore_MP.Nomenclatore_Settore = Nomenclatore_Settori.Codice LEFT OUTER JOIN
                         Nomenclatore ON Nomenclatore_Settori.Codice_Nomenclatore = Nomenclatore.Chiave LEFT OUTER JOIN
                         Anag_Prove ON Nomenclatore.Codice_Prova = Anag_Prove.Codice LEFT OUTER JOIN
                         Anag_Tecniche ON Nomenclatore.Codice_Tecnica = Anag_Tecniche.Codice LEFT OUTER JOIN
                         Laboratori_Reparto ON Esami_Aggregati.RepLab_analisi = Laboratori_Reparto.Chiave LEFT OUTER JOIN
                         Anag_Reparti ON Laboratori_Reparto.Reparto = Anag_Reparti.Codice LEFT OUTER JOIN
                         Anag_Laboratori ON Laboratori_Reparto.Laboratorio = Anag_Laboratori.Codice INNER JOIN
                         Indice_Campioni_Esaminati ON Esami_Aggregati.Anno_Conferimento = Indice_Campioni_Esaminati.Anno_Conferimento AND Esami_Aggregati.Numero_Conferimento = Indice_Campioni_Esaminati.Numero_Conferimento AND 
                         Esami_Aggregati.Codice = Indice_Campioni_Esaminati.Codice ON Risultati_Analisi.Anno_Conferimento = Indice_Campioni_Esaminati.Anno_Conferimento AND 
                         Risultati_Analisi.Numero_Conferimento = Indice_Campioni_Esaminati.Numero_Conferimento AND Risultati_Analisi.Codice = Indice_Campioni_Esaminati.Codice AND 
                         Risultati_Analisi.Numero_Campione = Indice_Campioni_Esaminati.Numero_Campione LEFT OUTER JOIN
                         Nomenclatore_Range ON Risultati_Analisi.Range = Nomenclatore_Range.Codice LEFT OUTER JOIN
                         Risultati_Analisi_Dettagli ON Indice_Campioni_Esaminati.Anno_Conferimento = Risultati_Analisi_Dettagli.Anno_Conferimento AND 
                         Indice_Campioni_Esaminati.Numero_Conferimento = Risultati_Analisi_Dettagli.Numero_Conferimento AND Indice_Campioni_Esaminati.Numero_Campione = Risultati_Analisi_Dettagli.Numero_Campione AND 
                         Indice_Campioni_Esaminati.Codice = Risultati_Analisi_Dettagli.Codice_Programmazione LEFT OUTER JOIN
                         Anag_Dettagli AS dbo_Anag_Dettagli_Diagnostica ON Risultati_Analisi_Dettagli.Codice_Dettaglio = dbo_Anag_Dettagli_Diagnostica.Codice LEFT OUTER JOIN
                         Anag_Tipo_Dett AS dbo_Anag_Tipo_Dett_Diagnostica ON Risultati_Analisi_Dettagli.Tipo_Dettaglio = dbo_Anag_Tipo_Dett_Diagnostica.Codice
WHERE        (Esami_Aggregati.Esame_Altro_Ente = 0) AND (Esami_Aggregati.Esame_Altro_Ente = 0) AND ({ fn YEAR(Conferimenti.Data) } >= 2021) AND (dbo_Anag_Reparti_ConfAcc.Descrizione IN  ('Reparto Virologia', 'Reparto Virologia (ME)', 'Reparto Virologia - Laboratorio Proteomica')) AND 
                         (Anag_Reparti.Descrizione NOT IN  ('Reparto Virologia', 'Reparto Virologia (ME)', 'Reparto Virologia - Laboratorio Proteomica'))


"


AbR <- "SELECT
  dbo.Conferimenti.Data_Prelievo,
  dbo.Conferimenti.Data,
  dbo.Conferimenti.Data_Accettazione,
  dbo.Conferimenti.Allevix_Proprietario,
  dbo.Anag_Specie.Descrizione As specie,
  dbo.Anag_Materiali.Descrizione As materiale,
  dbo_Anag_Dettagli_Antib_AgEziol.Dettaglio As agezio,
  dbo_Anag_Dettagli_Antib_Antib.Dettaglio As antib,
  dbo_Anag_Dettagli_Specifica_Antib.Risultato As esito,
  dbo.Conferimenti.Numero,
  dbo.Indice_Campioni_Esaminati.Numero_Campione
FROM
{ oj dbo.Anag_Specie RIGHT OUTER JOIN dbo.Conferimenti ON ( dbo.Anag_Specie.Codice=dbo.Conferimenti.Codice_Specie )
   INNER JOIN dbo.Anag_Comuni ON ( dbo.Anag_Comuni.Codice=dbo.Conferimenti.Luogo_Prelievo )
   LEFT OUTER JOIN dbo.Anag_Regioni ON ( dbo.Anag_Regioni.Codice=dbo.Anag_Comuni.Regione )
   LEFT OUTER JOIN dbo.Esami_Aggregati ON ( dbo.Conferimenti.Anno=dbo.Esami_Aggregati.Anno_Conferimento and dbo.Conferimenti.Numero=dbo.Esami_Aggregati.Numero_Conferimento )
   LEFT OUTER JOIN dbo.Nomenclatore_MP ON ( dbo.Esami_Aggregati.Nomenclatore=dbo.Nomenclatore_MP.Codice )
   LEFT OUTER JOIN dbo.Nomenclatore_Settori ON ( dbo.Nomenclatore_MP.Nomenclatore_Settore=dbo.Nomenclatore_Settori.Codice )
   LEFT OUTER JOIN dbo.Nomenclatore ON ( dbo.Nomenclatore_Settori.Codice_Nomenclatore=dbo.Nomenclatore.Chiave )
   LEFT OUTER JOIN dbo.Anag_Prove ON ( dbo.Nomenclatore.Codice_Prova=dbo.Anag_Prove.Codice )
   LEFT OUTER JOIN dbo.Laboratori_Reparto ON ( dbo.Esami_Aggregati.RepLab_analisi=dbo.Laboratori_Reparto.Chiave )
   LEFT OUTER JOIN dbo.Anag_Reparti ON ( dbo.Laboratori_Reparto.Reparto=dbo.Anag_Reparti.Codice )
   INNER JOIN dbo.Indice_Campioni_Esaminati ON ( dbo.Esami_Aggregati.Anno_Conferimento=dbo.Indice_Campioni_Esaminati.Anno_Conferimento and dbo.Esami_Aggregati.Numero_Conferimento=dbo.Indice_Campioni_Esaminati.Numero_Conferimento and dbo.Esami_Aggregati.Codice=dbo.Indice_Campioni_Esaminati.Codice )
   LEFT OUTER JOIN dbo.Risultati_Antib_Aggregati ON ( dbo.Indice_Campioni_Esaminati.Anno_Conferimento=dbo.Risultati_Antib_Aggregati.Anno_Conferimento and dbo.Indice_Campioni_Esaminati.Numero_Conferimento=dbo.Risultati_Antib_Aggregati.Numero_Conferimento and dbo.Indice_Campioni_Esaminati.Codice=dbo.Risultati_Antib_Aggregati.Codice_Programmazione and dbo.Indice_Campioni_Esaminati.Numero_Campione=dbo.Risultati_Antib_Aggregati.Nr_Campione )
   LEFT OUTER JOIN dbo.Anag_Dettagli  dbo_Anag_Dettagli_Antib_AgEziol ON ( dbo.Risultati_Antib_Aggregati.Cod_AgEziol=dbo_Anag_Dettagli_Antib_AgEziol.Codice )
   LEFT OUTER JOIN dbo.Anag_Dettagli  dbo_Anag_Dettagli_Antib_Antib ON ( dbo.Risultati_Antib_Aggregati.Cod_Antib=dbo_Anag_Dettagli_Antib_Antib.Codice )
   LEFT OUTER JOIN dbo.Anag_Dettagli_Specifica  dbo_Anag_Dettagli_Specifica_Antib ON ( dbo.Risultati_Antib_Aggregati.Cod_Esito=dbo_Anag_Dettagli_Specifica_Antib.Codice )
   INNER JOIN dbo.Anag_Registri ON ( dbo.Conferimenti.Registro=dbo.Anag_Registri.Codice )
   LEFT OUTER JOIN dbo.Anag_Materiali ON ( dbo.Anag_Materiali.Codice=dbo.Conferimenti.Codice_Materiale )
  }
WHERE
  dbo.Esami_Aggregati.Esame_Altro_Ente = 0
  AND  dbo.Esami_Aggregati.Esame_Altro_Ente = 0
  AND  (
  dbo.Anag_Prove.Descrizione  IN  ('Antibiogramma')
  AND  dbo.Anag_Registri.Descrizione  IN  ('Sanità Animale')
  AND  dbo.Anag_Reparti.Descrizione  =  'Sede Territoriale di Modena'
  AND  {fn year(dbo.Conferimenti.Data_Accettazione)}  >=  2021)"
























# queryFin <- "SELECT
#   dbo.Conferimenti.Numero,
#   dbo_Anag_Finalita_Confer.Descrizione
# FROM
#   dbo.Conferimenti,
#   dbo.Anag_Finalita  dbo_Anag_Finalita_Confer,
#   dbo.Anag_Registri,
#   dbo.Conferimenti_Finalita
# WHERE
#   ( dbo.Conferimenti.Registro=dbo.Anag_Registri.Codice  )
#   AND  ( dbo.Conferimenti.Anno=dbo.Conferimenti_Finalita.Anno and dbo.Conferimenti.Numero=dbo.Conferimenti_Finalita.Numero  )
#   AND  ( dbo.Conferimenti_Finalita.Finalita=dbo_Anag_Finalita_Confer.Codice  )
#   AND  (
#   {fn year(dbo.Conferimenti.Data_Accettazione)}  =  2021
#   AND  dbo.Anag_Registri.Descrizione  NOT IN  ('Altri Controlli (cosmetici,ambientali..)', 'Controlli Interni Sistema Qualità')
#   )
# "