# Proyecto 1 OHWe 2026 | Funciones reutilizables para OBIS
# -----------------------------------------------------------------------------
# obis_validate_config()
# Objetivo: Valida el dominio espacial, el intervalo de años y el indicador de descarga.
# Entradas: p: lista de parámetros del QMD (lon_min, lon_max, lat_min, lat_max, year_start, year_end, execute_download).
# Devuelve: TRUE invisible si los parámetros son consistentes; detiene la ejecución si fallan.
# -----------------------------------------------------------------------------
obis_validate_config <- function(p) {
  required <- c("lon_min", "lon_max", "lat_min", "lat_max", "year_start", "year_end", "execute_download")
  if (!all(required %in% names(p))) stop("Parámetros faltantes: ", paste(setdiff(required, names(p)), collapse = ", "))
  a <- unlist(p[c("lon_min", "lon_max", "lat_min", "lat_max", "year_start", "year_end")], use.names = FALSE)
  if (!all(is.finite(as.numeric(a)))) stop("Los límites y años deben ser numéricos finitos")
  stopifnot(p$lon_min >= -180, p$lon_max <= 180, p$lon_min < p$lon_max,
            p$lat_min >= -90, p$lat_max <= 90, p$lat_min < p$lat_max,
            p$year_start <= p$year_end,
            p$year_start == as.integer(p$year_start),
            p$year_end == as.integer(p$year_end))
  invisible(TRUE)
}

# -----------------------------------------------------------------------------
# obis_project_root()
# Objetivo: Identifica automáticamente la carpeta raíz del proyecto local.
# Entradas: No recibe argumentos; comprueba si R/, raw/, cin/ y cout/ existen en el directorio actual o en su padre.
# Devuelve: Ruta absoluta normalizada de proyecto/. No crea la estructura raíz.
# -----------------------------------------------------------------------------
obis_project_root <- function() {
  needed <- c("R", "raw", "cin", "cout")
  if (all(dir.exists(needed))) return(normalizePath(".", winslash = "/"))
  if (all(dir.exists(file.path("..", needed)))) return(normalizePath("..", winslash = "/"))
  stop("Ejecuta desde proyecto/ o proyecto/R/; faltan las carpetas R, raw, cin o cout")
}

# -----------------------------------------------------------------------------
# obis_paths()
# Objetivo: Construye y crea las subcarpetas OBIS dentro de raw/, cin/ y cout/.
# Entradas: root: ruta absoluta de la carpeta proyecto/.
# Devuelve: Lista de rutas raw, cin, tab y fig. Solo crea carpetas, no archivos.
# -----------------------------------------------------------------------------
obis_paths <- function(root) {
  paths <- list(raw = file.path(root,"raw","obis"),
                cin = file.path(root,"cin","obis"),
                tab = file.path(root,"cout","obis","tab"),
                fig = file.path(root,"cout","obis","fig"))
  invisible(lapply(paths, dir.create, recursive = TRUE, showWarnings = FALSE))
  paths
}

# -----------------------------------------------------------------------------
# obis_wkt()
# Objetivo: Construye el rectángulo geográfico de consulta en Well-Known Text (WKT).
# Entradas: p: parámetros de longitud y latitud mínima/máxima.
# Devuelve: Cadena POLYGON con pares X=longitud, Y=latitud. No es una máscara marina.
# -----------------------------------------------------------------------------
obis_wkt <- function(p) {
  sprintf("POLYGON((%.8f %.8f, %.8f %.8f, %.8f %.8f, %.8f %.8f, %.8f %.8f))",
          p$lon_min,p$lat_min,p$lon_max,p$lat_min,p$lon_max,p$lat_max,
          p$lon_min,p$lat_max,p$lon_min,p$lat_min)
}

# -----------------------------------------------------------------------------
# obis_write_csv()
# Objetivo: Guarda una tabla CSV de forma segura mediante un archivo temporal y reemplazo controlado.
# Entradas: d: data.frame; path: ruta de destino.
# Devuelve: Ruta de destino (invisible). Si existe un respaldo .previous no lo sobrescribe.
# -----------------------------------------------------------------------------
obis_write_csv <- function(d, path) {
  dir.create(dirname(path),recursive=TRUE,showWarnings=FALSE)
  tmp <- tempfile(pattern=".pending_",tmpdir=dirname(path),fileext=".csv")
  on.exit(unlink(tmp),add=TRUE)
  readr::write_csv(d,tmp,na="")
  if (file.exists(path)) {
    backup <- paste0(path,".previous")
    if (file.exists(backup)) stop("Existe respaldo pendiente; revisar antes de reemplazar: ", backup)
    if (!file.rename(path,backup)) stop("No se pudo respaldar: ",path)
    if (!file.rename(tmp,path)) { file.rename(backup,path);stop("Fallo de reemplazo: ",path) }
    unlink(backup)
  } else if (!file.rename(tmp,path)) stop("No se pudo guardar: ",path)
  invisible(path)
}

# -----------------------------------------------------------------------------
# obis_flatten()
# Objetivo: Convierte a columnas de texto las columnas tipo lista de la respuesta OBIS.
# Entradas: x: data.frame devuelto por robis::occurrence().
# Devuelve: Tibble apto para exportación CSV; los elementos de listas se unen con punto y coma.
# -----------------------------------------------------------------------------
obis_flatten <- function(x) {
  x <- tibble::as_tibble(x)
  for (nm in names(x)) if (is.list(x[[nm]])) {
    x[[nm]] <- vapply(x[[nm]],function(z) paste(as.character(unlist(z)),collapse=";"),character(1))
  }
  x
}

# -----------------------------------------------------------------------------
# obis_read_raw_csv()
# Objetivo: Lee un CSV original de OBIS sin inferencia de tipos para preservar los campos tal como llegaron.
# Entradas: path: archivo raw/obis/obis_AAAA.csv.
# Devuelve: Tibble con columnas de tipo carácter; detiene el proceso ante problemas de lectura.
# -----------------------------------------------------------------------------
obis_read_raw_csv <- function(path) {
  d <- readr::read_csv(path,col_types=readr::cols(.default=readr::col_character()),
                       show_col_types=FALSE,progress=FALSE)
  if (!is.data.frame(d)) stop("Archivo no tabular: ",path)
  if (length(readr::problems(d)$row)) stop("Problemas al leer CSV: ",path)
  d
}

# -----------------------------------------------------------------------------
# obis_download_year()
# Objetivo: Consulta las ocurrencias OBIS de un año dentro del polígono suministrado.
# Entradas: year: año calendario; wkt: polígono espacial en WKT.
# Devuelve: Data.frame de ocurrencias generado por robis. No guarda ni depura.
# -----------------------------------------------------------------------------
obis_download_year <- function(year, wkt) {
  robis::occurrence(geometry=wkt,
                    startdate=sprintf("%d-01-01",year),
                    enddate=sprintf("%d-12-31",year))
}

# -----------------------------------------------------------------------------
# obis_download_period()
# Objetivo: Gestiona las consultas anuales reanudables, usando un CSV original por año.
# Entradas: years: vector de años; wkt: polígono; raw_dir: carpeta destino; execute_download: activar llamadas remotas.
# Devuelve: Tibble manifiesto con año, estado, cantidad de registros y detalle de errores. Reutiliza CSV existentes.
# -----------------------------------------------------------------------------
obis_download_period <- function(years, wkt, raw_dir, execute_download=FALSE) {
  manifest <- lapply(years,function(yr) {
    path <- file.path(raw_dir,sprintf("obis_%d.csv",yr))
    status <- ""; n <- NA_integer_; detail <- ""
    if (file.exists(path)) {
      check <- tryCatch(obis_read_raw_csv(path),error=identity)
      if (inherits(check,"error")) {status <- "ERROR_CSV_EXISTENTE";detail <- conditionMessage(check)}
      else {status <- "EXISTENTE";n <- nrow(check)}
    } else if (!isTRUE(execute_download)) {
      status <- "SIN_CSV_NO_DESCARGADO"
    } else {
      message("Descargando OBIS: ",yr)
      result <- tryCatch(obis_download_year(yr,wkt),error=identity)
      if (inherits(result,"error")) {status <- "ERROR_API";detail <- conditionMessage(result)}
      else if (!is.data.frame(result)) status <- "FORMATO_INVALIDO"
      else {
        result <- obis_flatten(result)
        saved <- tryCatch({obis_write_csv(result,path);TRUE},error=identity)
        if (inherits(saved,"error")) {status <- "ERROR_GUARDADO";detail <- conditionMessage(saved)}
        else {status <- "DESCARGADO";n <- nrow(result)}
      }
    }
    tibble::tibble(year=yr,status=status,records=n,detail=detail)
  })
  dplyr::bind_rows(manifest)
}

# -----------------------------------------------------------------------------
# obis_integrate_raw()
# Objetivo: Integra con bind_rows() todos los CSV anuales originales, antes de cualquier depuración.
# Entradas: manifest: estados de descarga; years: años esperados; raw_dir: carpeta de archivos originales.
# Devuelve: Tabla integrada con source_year para trazabilidad o NULL si falta algún año.
# -----------------------------------------------------------------------------
obis_integrate_raw <- function(manifest,years,raw_dir) {
  ok <- nrow(manifest)==length(years) && all(manifest$status %in% c("EXISTENTE","DESCARGADO"))
  if (!ok) return(NULL)
  pieces <- lapply(years,function(yr) {
    d <- obis_read_raw_csv(file.path(raw_dir,sprintf("obis_%d.csv",yr)))
    d$source_year <- as.character(yr)
    d
  })
  combined <- dplyr::bind_rows(pieces)
  stopifnot(nrow(combined)==sum(manifest$records))
  combined
}

# -----------------------------------------------------------------------------
# obis_col_text()
# Objetivo: Extrae una columna del data.frame como carácter, sin fallar si no está presente.
# Entradas: x: tabla; nm: nombre de columna.
# Devuelve: Vector de caracteres; NA para campos inexistentes.
# -----------------------------------------------------------------------------
obis_col_text <- function(x,nm) if(nm %in% names(x)) as.character(x[[nm]]) else rep(NA_character_,nrow(x))
# -----------------------------------------------------------------------------
# obis_col_num()
# Objetivo: Convierte una columna a numérico, partiendo de la extracción segura de texto.
# Entradas: x: tabla; nm: nombre de columna.
# Devuelve: Vector numérico; valores no convertibles se representan como NA.
# -----------------------------------------------------------------------------
obis_col_num <- function(x,nm) suppressWarnings(as.numeric(obis_col_text(x,nm)))
# -----------------------------------------------------------------------------
# obis_first_text()
# Objetivo: Obtiene el primer valor textual no vacío entre varias columnas alternativas.
# Entradas: x: tabla; names: nombres de columnas ordenados por prioridad.
# Devuelve: Vector de caracteres; NA cuando todos los candidatos faltan.
# -----------------------------------------------------------------------------
obis_first_text <- function(x,names) {
  ans <- rep(NA_character_,nrow(x))
  for(nm in names) {
    v <- trimws(obis_col_text(x,nm));v[v==""] <- NA_character_
    ans[is.na(ans)] <- v[is.na(ans)]
  }
  ans
}
# -----------------------------------------------------------------------------
# obis_true()
# Objetivo: Interpreta representaciones textuales habituales de valores booleanos positivos.
# Entradas: x: vector con TRUE/FALSE o valores de texto como true, t, 1 y yes.
# Devuelve: Vector lógico; faltantes y representaciones no reconocidas no se consideran TRUE.
# -----------------------------------------------------------------------------
obis_true <- function(x) tolower(trimws(as.character(x))) %in% c("true","t","1","yes")

# -----------------------------------------------------------------------------
# obis_clean_standardize()
# Objetivo: Aplica filtros de calidad, deduplica por identificador OBIS y estandariza campos.
# Entradas: raw_all: tabla original integrada; p: dominio y años válidos.
# Devuelve: Lista clean (base para análisis), quality (marcas por fila), audit (exclusiones) y missing_fields (completitud). Exige identificación a especie y afinidad marina confirmada para la base central; no interpreta ausencia de registros como ausencia biológica.
# -----------------------------------------------------------------------------
obis_clean_standardize <- function(raw_all,p) {
  if (is.null(raw_all)) return(NULL)
  n <- nrow(raw_all)
  lon <- obis_col_num(raw_all,"decimalLongitude"); lat <- obis_col_num(raw_all,"decimalLatitude")
  yy <- obis_col_num(raw_all,"date_year"); fallback <- obis_col_num(raw_all,"year")
  yy[is.na(yy)] <- fallback[is.na(yy)]
  mm <- obis_col_num(raw_all,"month"); dd <- obis_col_num(raw_all,"day")
  basis <- toupper(trimws(obis_col_text(raw_all,"basisOfRecord")))
  fossil <- !is.na(basis) & grepl("FOSSIL",basis)
  absent <- obis_true(obis_col_text(raw_all,"absence")) |
    toupper(trimws(obis_col_text(raw_all,"occurrenceStatus"))) %in% c("ABSENT","ABSENCE")
  dropped <- obis_true(obis_col_text(raw_all,"dropped"))
  marine <- obis_true(obis_col_text(raw_all,"marine"))
  flags <- obis_col_text(raw_all,"flags")
  on_land <- !is.na(flags) & grepl("ON_LAND|ON LAND",flags,ignore.case=TRUE)
  valid_time <- is.finite(yy) & yy>=p$year_start & yy<=p$year_end & yy==floor(yy) &
    yy==obis_col_num(raw_all,"source_year")
  valid_xy <- is.finite(lon) & is.finite(lat) & lon>=p$lon_min & lon<=p$lon_max &
    lat>=p$lat_min & lat<=p$lat_max
  # Un registro identificado solo a género/familia se audita pero no ingresa al núcleo específico.
  species <- obis_first_text(raw_all,"species")
  valid_species <- !is.na(species)
  # Cada observación recibe su primera causa de exclusión según el orden
  # de filtros; ello permite una auditoría sin doble conteo de exclusiones.
  reason <- rep("RETIENE",n)
  set_reason <- function(which,label) {idx <- reason=="RETIENE" & (which %in% TRUE);reason[idx] <<- label}
  set_reason(!valid_time,"FECHA_FUERA_O_SIN_ANIO")
  set_reason(!valid_xy,"COORDENADAS_INVALIDAS_O_FUERA")
  set_reason(absent,"AUSENCIA_EXPLICITA")
  set_reason(fossil,"REGISTRO_FOSIL")
  set_reason(dropped,"DESCARTADO_OBIS")
  set_reason(on_land,"ALERTA_TIERRA")
  set_reason(!marine,"AFINIDAD_MARINA_NO_CONFIRMADA")
  set_reason(!valid_species,"NO_IDENTIFICADO_A_ESPECIE")
  quality <- tibble::tibble(source_year=obis_col_num(raw_all,"source_year"),
    reason=reason,year_data=yy,month=mm,valid_species=valid_species,
    marine=marine,fossil=fossil,absence=absent,dropped=dropped,flags=flags)
  core <- raw_all[reason=="RETIENE",,drop=FALSE]
  sel <- reason=="RETIENE"
  core$source <- rep("OBIS",nrow(core));core$year_analysis <- as.integer(yy[sel]);core$month_analysis <- as.integer(mm[sel]);
  core$day_analysis <- as.integer(dd[sel]);core$decimalLongitude <- lon[sel];
  core$decimalLatitude <- lat[sel];core$species_accepted <- species[sel]
  # Un ID técnico OBIS repetido representa la misma fila; nunca deduplicar solo por lat/lon/fecha/taxón.
  ids <- obis_first_text(core,"id")
  # Solo se eliminan IDs OBIS repetidos; la coincidencia de especie,
  # fecha y coordenadas NO basta para identificar observaciones duplicadas.
  duplicated_id <- !is.na(ids) & duplicated(ids)
  core <- core[!duplicated_id,,drop=FALSE]
  keep <- c("source","source_year","year_analysis","month_analysis","day_analysis",
    "eventDate","date_start","date_end","decimalLongitude","decimalLatitude",
    "coordinateUncertaintyInMeters","depth","minimumDepthInMeters","maximumDepthInMeters",
    "kingdom","phylum","class","order","family","genus","species","species_accepted",
    "scientificName","originalScientificName","acceptedScientificName","aphiaID","speciesid",
    "taxonRank","taxonomicStatus","marine","brackish","basisOfRecord","occurrenceStatus",
    "id","occurrenceID","eventID","dataset_id","datasetID","datasetName",
    "catalogNumber","institutionCode","samplingProtocol","individualCount","flags","license")
  for(nm in setdiff(keep,names(core))) core[[nm]] <- rep(NA_character_,nrow(core))
  clean <- core[,keep,drop=FALSE]
  audit <- quality |>
    dplyr::count(reason,name="n") |>
    dplyr::mutate(percent=round(100*n/max(nrow(raw_all),1),2)) |>
    dplyr::bind_rows(tibble::tibble(reason="DUPLICADO_ID_OBIS",n=sum(duplicated_id),
      percent=round(100*sum(duplicated_id)/max(nrow(raw_all),1),2)))
  list(clean=clean,quality=quality,audit=audit,
       missing_fields=tibble::tibble(variable=names(raw_all),
         n_nonmissing=vapply(raw_all,function(z)sum(!is.na(z)&trimws(as.character(z))!=""),integer(1))))
}

# -----------------------------------------------------------------------------
# obis_longest_run()
# Objetivo: Calcula la mayor secuencia de años consecutivos con registros.
# Entradas: ys: vector de años válidos de un grupo taxonómico.
# Devuelve: Número entero de años consecutivos; cero si no hay fechas.
# -----------------------------------------------------------------------------
obis_longest_run <- function(ys) {
  ys <- sort(unique(ys[is.finite(ys)]))
  if(!length(ys)) return(0L)
  max(rle(cumsum(c(1,diff(ys)!=1)))$lengths)
}

# -----------------------------------------------------------------------------
# obis_taxonomic_screening()
# Objetivo: Resume la representatividad taxonómica y temporal por filo, clase, orden y familia.
# Entradas: clean: registros depurados; years: periodo estudiado.
# Devuelve: Lista representativity (indicadores/candidatos) y yearly_taxa (conteos por año y taxón). El filtro coverage_screen es exploratorio, no una validación de diversidad.
# -----------------------------------------------------------------------------
obis_taxonomic_screening <- function(clean,years) {
  if (is.null(clean)||!nrow(clean)) return(NULL)
  dx <- dplyr::mutate(clean,
    site_diagnostic=paste(round(decimalLongitude,2),round(decimalLatitude,2),sep="/"),
    dataset_key=dplyr::coalesce(dplyr::na_if(as.character(dataset_id),""),
      dplyr::na_if(as.character(datasetID),""),"DESCONOCIDO"),
    temporal_block=as.integer(floor((year_analysis-min(years))/5))+1L)
  ranks <- c("phylum","class","order","family")
  groups <- lapply(ranks,function(rank) {
    dx |>
      dplyr::mutate(group=trimws(as.character(.data[[rank]]))) |>
      dplyr::filter(!is.na(group),group!="") |>
      dplyr::group_by(group) |>
      dplyr::summarise(rank=rank,records=dplyr::n(),
        species=dplyr::n_distinct(species_accepted,na.rm=TRUE),
        years_present=dplyr::n_distinct(year_analysis),
        first_year=min(year_analysis),last_year=max(year_analysis),
        longest_consecutive_run=obis_longest_run(year_analysis),
        months_present=dplyr::n_distinct(month_analysis[month_analysis %in% 1:12]),
        datasets=dplyr::n_distinct(dataset_key),
        dominant_dataset_share=max(table(dataset_key))/dplyr::n(),
        distinct_locations=dplyr::n_distinct(site_diagnostic),
        five_year_blocks=dplyr::n_distinct(temporal_block),.groups="drop")
  })
  representativity <- dplyr::bind_rows(groups) |>
    dplyr::mutate(fraction_years=round(years_present/length(years),3),
      dominant_dataset_share=round(dominant_dataset_share,3),
      # Umbrales orientativos para explorar continuidad y procedencia.
      # No implican muestreo comparable ni permiten inferir biodiversidad.
      coverage_screen=years_present>=ceiling(.5*length(years)) &
        species>=5 & datasets>=3 & distinct_locations>=10 & dominant_dataset_share<=.8) |>
    dplyr::arrange(rank,dplyr::desc(years_present),dplyr::desc(records))
  yearly_taxa <- dplyr::bind_rows(lapply(c(ranks,"genus"),function(rank) {
    dx |>
      dplyr::mutate(taxon=as.character(.data[[rank]])) |>
      dplyr::filter(!is.na(taxon),trimws(taxon)!="") |>
      dplyr::count(year_analysis,taxon,name="records") |>
      dplyr::mutate(rank=rank)
  })) |>
    dplyr::select(rank,year_analysis,taxon,records)
  list(representativity=representativity,yearly_taxa=yearly_taxa)
}

# -----------------------------------------------------------------------------
# obis_eda_tables()
# Objetivo: Produce los conteos de ocurrencias de la base limpia para gráficos exploratorios.
# Entradas: clean: tabla estandarizada con year_analysis y month_analysis.
# Devuelve: Lista yearly, monthly y year_month. Son registros, no abundancia ni índices de diversidad.
# -----------------------------------------------------------------------------
obis_eda_tables <- function(clean) {
  if (is.null(clean)||!nrow(clean)) return(NULL)
  list(yearly=dplyr::count(clean,year_analysis,name="records"),
       monthly=dplyr::count(dplyr::filter(clean,month_analysis %in% 1:12),month_analysis,name="records"),
       year_month=dplyr::count(dplyr::filter(clean,month_analysis %in% 1:12),year_analysis,month_analysis,name="records"))
}

# =============================================================================
# EXTENSIÓN: EVALUACIÓN DE IDONEIDAD ESPACIAL Y TAXONÓMICA
# =============================================================================
# Estas funciones no alteran ni sobrescriben los datos originales ni la base
# estandarizada. Operan sobre el objeto 'clean' ya generado por main.R.
# Los conteos de ocurrencias OBIS NO equivalen a abundancia o esfuerzo uniforme.

# -----------------------------------------------------------------------------
# obis_spatial_screening()
# Objetivo: evaluar la cobertura espacial de cada grupo por año mediante una
#           cuadrícula diagnóstica configurable, SIN modificar coordenadas.
# Entradas: clean (base estandarizada), p (lon_min, lon_max, lat_min, lat_max),
#           grid_res (resolución en grados), ranks (niveles taxonómicos).
# Devuelve: lista con cell_counts (registros por celda-año-taxon), annual
#           (indicadores por taxón-año) y summary (indicadores multianuales).
# Precaución: n_cells significa 'celdas con ocurrencias documentadas'; una
#             celda sin datos NO equivale a ausencia biológica.
# -----------------------------------------------------------------------------
obis_spatial_screening <- function(clean, p, grid_res = 0.25,
                                   ranks = c("phylum", "class", "order")) {
  if (is.null(clean) || !nrow(clean)) return(NULL)
  if (length(grid_res) != 1L || !is.numeric(grid_res) ||
      !is.finite(grid_res) || grid_res <= 0 || grid_res > 5) {
    stop("grid_res debe ser un número positivo de grados (<= 5)")
  }
  stopifnot(all(ranks %in% names(clean)))

  # Anclar al extremo suroccidental del dominio, sin redondear registros.
  nx <- ceiling((p$lon_max - p$lon_min) / grid_res)
  ny <- ceiling((p$lat_max - p$lat_min) / grid_res)
  ix <- floor((clean$decimalLongitude - p$lon_min) / grid_res)
  iy <- floor((clean$decimalLatitude - p$lat_min) / grid_res)
  ix <- pmin(nx - 1L, pmax(0L, as.integer(ix)))
  iy <- pmin(ny - 1L, pmax(0L, as.integer(iy)))

  dx <- clean |>
    dplyr::mutate(
      event_key = dplyr::if_else(
        !is.na(.data$eventID) & trimws(.data$eventID) != "" &
          !is.na(.data$dataset_id) & trimws(.data$dataset_id) != "",
        paste(.data$dataset_id, .data$eventID, sep = "|"),
        NA_character_
      ),
      cell_x = ix,
      cell_y = iy,
      cell_id = sprintf("C%03d_%03d", cell_x, cell_y),
      cell_lon_min = p$lon_min + cell_x * grid_res,
      cell_lat_min = p$lat_min + cell_y * grid_res
    )

  cell_counts <- dplyr::bind_rows(lapply(ranks, function(nivel) {
    dx |>
      dplyr::mutate(taxon = trimws(as.character(.data[[nivel]]))) |>
      dplyr::filter(!is.na(.data$taxon), .data$taxon != "") |>
      dplyr::group_by(.data$year_analysis, .data$taxon,
                      .data$cell_id, .data$cell_lon_min, .data$cell_lat_min) |>
      dplyr::summarise(
        records_cell = dplyr::n(),
        species_cell = dplyr::n_distinct(.data$species_accepted, na.rm = TRUE),
        identifiable_events_cell = dplyr::n_distinct(.data$event_key, na.rm = TRUE),
        records_with_event_id = sum(!is.na(.data$event_key)),
        .groups = "drop"
      ) |>
      dplyr::mutate(rank = nivel)
  })) |>
    dplyr::select(
      dplyr::all_of(c(
        "rank",
        "taxon",
        "year_analysis",
        "cell_id",
        "cell_lon_min",
        "cell_lat_min",
        "records_cell",
        "species_cell",
        "identifiable_events_cell",
        "records_with_event_id"
      ))
    )

  annual <- cell_counts |>
    dplyr::group_by(.data$rank, .data$taxon, .data$year_analysis) |>
    dplyr::summarise(
      records_total = sum(.data$records_cell),
      n_cells = dplyr::n(),
      mean_records_per_cell = mean(.data$records_cell),
      median_records_per_cell = stats::median(.data$records_cell),
      max_cell_share = max(.data$records_cell) / sum(.data$records_cell),
      n_cells_ge_5 = sum(.data$records_cell >= 5L),
      n_cells_ge_10 = sum(.data$records_cell >= 10L),
      event_annotated_fraction = sum(.data$records_with_event_id) / sum(.data$records_cell),
      effective_cells = 1 / sum((.data$records_cell / sum(.data$records_cell))^2),
      .groups = "drop"
    ) |>
    dplyr::arrange(.data$rank, .data$taxon, .data$year_analysis)

  # Solo incluye años con observaciones. Se calculan medianas sobre años observados;
  # los años sin registros se mantienen fuera y se cuentan separadamente.
  summary <- annual |>
    dplyr::group_by(.data$rank, .data$taxon) |>
    dplyr::summarise(
      years_observed_spatial = dplyr::n_distinct(.data$year_analysis),
      total_records = sum(.data$records_total),
      median_cells_year = stats::median(.data$n_cells),
      mean_cells_year = mean(.data$n_cells),
      median_max_cell_share = stats::median(.data$max_cell_share),
      mean_max_cell_share = mean(.data$max_cell_share),
      median_event_annotated_fraction = stats::median(.data$event_annotated_fraction),
      median_effective_cells = stats::median(.data$effective_cells),
      years_ge_5_cells = sum(.data$n_cells >= 5L),
      years_ge_10_cells = sum(.data$n_cells >= 10L),
      .groups = "drop"
    ) |>
    dplyr::arrange(.data$rank, dplyr::desc(.data$total_records))

  list(cell_counts = cell_counts, annual = annual, summary = summary)
}

# -----------------------------------------------------------------------------
# obis_plot_spatial_facets()
# Objetivo: generar facetas ordenadas de mayor a menor por registros acumulados.
# Entradas: annual (indicadores por año), rank_value (phylum/class/order),
#           metric (n_cells, max_cell_share o mean_records_per_cell), n_top.
# Devuelve: objeto ggplot con theme_cowplot(), o NULL sin datos.
# Precaución: las líneas solo conectan años con registros; no se imputan ceros.
# -----------------------------------------------------------------------------
obis_plot_spatial_facets <- function(annual, rank_value,
                                     metric = "n_cells", n_top = 12L) {
  metrics <- c("n_cells", "max_cell_share", "mean_records_per_cell")
  if (!metric %in% metrics) stop("Métrica no reconocida: ", metric)
  d <- annual |>
    dplyr::filter(.data$rank == .env$rank_value)
  if (!nrow(d)) return(NULL)

  leaders <- d |>
    dplyr::group_by(.data$taxon) |>
    dplyr::summarise(total = sum(.data$records_total), .groups = "drop") |>
    dplyr::arrange(dplyr::desc(.data$total), .data$taxon) |>
    dplyr::slice_head(n = n_top)

  d <- d |>
    dplyr::filter(.data$taxon %in% .env$leaders$taxon) |>
    dplyr::arrange(.data$taxon, .data$year_analysis) |>
    dplyr::group_by(.data$taxon) |>
    dplyr::mutate(
      # No unir con una línea años separados por periodos sin registros.
      series_id = cumsum(c(1L, as.integer(diff(.data$year_analysis) != 1L)))
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(taxon = factor(.data$taxon, levels = leaders$taxon))

  label_y <- c(
    n_cells = "Celdas con registros",
    max_cell_share = "Proporción en la celda dominante",
    mean_records_per_cell = "Registros por celda con datos (promedio)"
  )
  label_title <- c(
    n_cells = "Cobertura espacial anual",
    max_cell_share = "Concentración espacial anual",
    mean_records_per_cell = "Intensidad documental por celda y año"
  )

  gp <- ggplot2::ggplot(d,
      ggplot2::aes(x = .data$year_analysis, y = .data[[metric]],
                   group = interaction(.data$taxon, .data$series_id))) +
    ggplot2::geom_line(linewidth = 0.65, colour = "#222222") +
    ggplot2::geom_point(size = 1.0, colour = "#222222") +
    ggplot2::facet_wrap(~ taxon, ncol = 3, scales = "free_y") +
    ggplot2::labs(
      title = paste0("OBIS: ", label_title[[metric]], " (", rank_value, ")"),
      subtitle = "Orden de facetas: mayor a menor total de registros",
      x = "Año", y = label_y[[metric]]
    ) +
    cowplot::theme_cowplot(font_size = 11) +
    ggplot2::theme(
      strip.text = ggplot2::element_text(face = "bold"),
      plot.title = ggplot2::element_text(face = "bold")
    )
  if (metric == "max_cell_share") {
    gp <- gp + ggplot2::scale_y_continuous(limits = c(0, 1))
  }
  gp
}

# -----------------------------------------------------------------------------
# obis_taxon_suitability()
# Objetivo: combinar cobertura temporal y espacial para una revisión científica
#           de clados candidatos, sin establecer un 'mejor taxón' automático.
# Entradas: representativity (screening existente) y spatial_summary.
# Devuelve: tabla de indicadores y banderas exploratorias para priorización.
# Precaución: los umbrales solo señalan candidatos para revisar, no garantizan
#             esfuerzo homogéneo, continuidad metodológica ni diversidad real.
# -----------------------------------------------------------------------------
obis_taxon_suitability <- function(representativity, spatial_summary) {
  if (is.null(representativity) || is.null(spatial_summary)) return(NULL)

  out <- representativity |>
    dplyr::filter(.data$rank %in% c("phylum", "class", "order")) |>
    dplyr::left_join(spatial_summary, by = c("rank", "group" = "taxon")) |>
    dplyr::mutate(
      spatial_screen = !is.na(.data$median_cells_year) &
        .data$median_cells_year >= 5 &
        .data$median_max_cell_share <= 0.6 &
        .data$years_ge_5_cells >= 5,
      joint_screen = .data$coverage_screen & .data$spatial_screen,
      review_priority = dplyr::case_when(
        .data$joint_screen ~ "REVISAR_PRIMERO",
        .data$coverage_screen | .data$spatial_screen ~ "REVISAR_CON_RESERVAS",
        TRUE ~ "INFORMACION_LIMITADA"
      )
    ) |>
    dplyr::arrange(dplyr::desc(.data$joint_screen),
                   dplyr::desc(.data$years_present),
                   dplyr::desc(.data$median_cells_year),
                   dplyr::desc(.data$records))
  out
}

# -----------------------------------------------------------------------------
# obis_rarefaction_screening()
# Objetivo: generar curvas de rarefacción por incidencia SOLO cuando existen
#           identificadores de eventos (eventID) dentro de un mismo dataset.
# Entradas: clean (base integrada), suitability (candidatos), ranks, max_groups.
# Devuelve: lista 'audit' (motivos de ejecución o descarte) y 'plots' (ggplots).
# Precaución: las curvas no estiman tendencias interanuales. Se usa UN dataset
#             por taxón; se omiten otros datasets para no juntar protocolos.
#             Exige varios eventos multiespecíficos, como diagnóstico mínimo.
# Dependencia adicional opcional: paquete iNEXT.
# -----------------------------------------------------------------------------
obis_rarefaction_screening <- function(clean, suitability,
                                        ranks = c("class", "order"),
                                        max_groups = 4L) {
  if (!requireNamespace("iNEXT", quietly = TRUE)) {
    stop("Para evaluar rarefacción instala iNEXT")
  }
  if (!is.numeric(max_groups) || length(max_groups) != 1 ||
      !is.finite(max_groups) || max_groups < 1) stop("max_groups inválido")

  ranks <- intersect(ranks, names(clean))
  # Distribuir cupos entre clases y órdenes antes de aplicar el tope global.
  per_rank <- ceiling(max_groups / length(ranks))
  chosen <- suitability |>
    dplyr::filter(.data$rank %in% .env$ranks) |>
    dplyr::arrange(dplyr::desc(.data$joint_screen),
                   dplyr::desc(.data$years_present),
                   dplyr::desc(.data$median_cells_year),
                   dplyr::desc(.data$records)) |>
    dplyr::group_by(.data$rank) |>
    dplyr::slice_head(n = per_rank) |>
    dplyr::ungroup() |>
    dplyr::slice_head(n = as.integer(max_groups))

  audit_rows <- list()
  plots <- list()

  for (k in seq_len(nrow(chosen))) {
    nivel <- as.character(chosen$rank[[k]])
    taxon_actual <- as.character(chosen$group[[k]])
    d <- clean |>
      dplyr::filter(!is.na(.data[[nivel]]),
                    .data[[nivel]] == .env$taxon_actual) |>
      dplyr::mutate(
        event = trimws(as.character(.data$eventID)),
        ds = dplyr::coalesce(
          dplyr::na_if(trimws(as.character(.data$dataset_id)), ""),
          dplyr::na_if(trimws(as.character(.data$datasetID)), "")
        ),
        sp = trimws(as.character(.data$species_accepted))
      ) |>
      dplyr::filter(!is.na(.data$event), .data$event != "",
                    !is.na(.data$ds), .data$ds != "",
                    !is.na(.data$sp), .data$sp != "")

    status <- "SIN_EVENTOS_VALIDOS"
    selected_ds <- NA_character_
    events <- 0L
    n_species <- 0L
    multi_rate <- NA_real_
    plot_key <- paste0(nivel, "_", gsub("[^A-Za-z0-9_-]", "_", taxon_actual))

    if (nrow(d)) {
      ds_count <- d |>
        dplyr::distinct(.data$ds, .data$event) |>
        dplyr::count(.data$ds, name = "n_events") |>
        dplyr::arrange(dplyr::desc(.data$n_events), .data$ds)
      selected_ds <- ds_count$ds[[1]]
      # Solo se utilizan incidencias únicas especie × evento del dataset focal.
      z <- d |>
        dplyr::filter(.data$ds == .env$selected_ds) |>
        dplyr::distinct(.data$event, .data$sp)
      events <- dplyr::n_distinct(z$event)
      n_species <- dplyr::n_distinct(z$sp)
      event_sizes <- z |>
        dplyr::count(.data$event, name = "n_species")
      multi_rate <- mean(event_sizes$n_species >= 2L)
      if (events < 10L || n_species < 3L) {
        status <- "EVENTOS_O_ESPECIES_INSUFICIENTES"
      } else if (multi_rate < 0.10) {
        status <- "EVENTOS_MULTIESPECIFICOS_INSUFICIENTES"
      } else if ((events * n_species) > 2e6) {
        status <- "MATRIZ_DE_INCIDENCIA_DEMASIADO_GRANDE"
      } else {
        # xtabs construye una matriz especies × eventos de presencia binaria.
        m <- stats::xtabs(~ sp + event, data = z)
        m <- 1L * (as.matrix(m) > 0)
        fit <- tryCatch(
          iNEXT::iNEXT(m, q = 0, datatype = "incidence_raw",
                       nboot = 0, knots = 25),
          error = identity
        )
        if (inherits(fit, "error")) {
          status <- paste0("ERROR_INEXT: ", conditionMessage(fit))
        } else {
          status <- "CURVA_EXPLORATORIA_GENERADA"
          gp <- iNEXT::ggiNEXT(fit, type = 1) +
            cowplot::theme_cowplot(font_size = 11) +
            ggplot2::labs(
              title = paste0("Rarefacción por incidencia: ", taxon_actual),
              subtitle = paste0("Nivel: ", nivel, " | Dataset: ", selected_ds),
              x = "Eventos documentados", y = "Riqueza de especies estimada"
            )
          plots[[plot_key]] <- gp
        }
      }
    }
    audit_rows[[k]] <- tibble::tibble(
      rank = nivel, taxon = taxon_actual,
      dataset_focal = selected_ds, events = events,
      species = n_species,
      fraction_multispecies_events = multi_rate,
      status = status
    )
  }
  list(audit = dplyr::bind_rows(audit_rows), plots = plots)
}

# =============================================================================
# EXTENSIÓN II — CARTOGRAFÍA, SUFICIENCIA ESPACIAL Y COBERTURA DE INVENTARIOS
# =============================================================================
# No reemplaza ninguno de los procedimientos anteriores. La cuadrícula es
# diagnóstica y no altera las coordenadas originales almacenadas en cin/.

# -----------------------------------------------------------------------------
# obis_selected_years()
# Objetivo: Seleccionar hasta n años repartidos en todo el periodo para mapas.
# Entradas: years, vector de años; n, cantidad máxima.
# Devuelve: años existentes y ordenados, sin inventar años sin observaciones.
# -----------------------------------------------------------------------------
obis_selected_years <- function(years, n = 4L) {
  years <- sort(unique(as.integer(years[!is.na(years)])))
  if (!length(years)) return(integer())
  years[unique(round(seq(1, length(years), length.out = min(n, length(years)))))]
}

# -----------------------------------------------------------------------------
# obis_map_tiles()
# Objetivo: Preparar información espacial por celda para mapas, sin recortar
#           ni modificar la base de ocurrencias.
# Entradas: spatial$cell_counts, rango taxonómico, resolución y tipo de mapa.
# Devuelve: tabla con posición central de celda, taxón y cantidad registrada.
# Nota: "records" es intensidad documental, no abundancia ni esfuerzo.
# -----------------------------------------------------------------------------
obis_map_tiles <- function(cell_counts, rank_value, grid_res,
                           mode = c("records", "years"), n_top = 6L) {
  mode <- match.arg(mode)
  selected <- cell_counts |>
    dplyr::filter(.data$rank == .env$rank_value)
  if (!nrow(selected)) return(NULL)
  leaders <- selected |>
    dplyr::group_by(.data$taxon) |>
    dplyr::summarise(total = sum(.data$records_cell), .groups = "drop") |>
    dplyr::arrange(dplyr::desc(.data$total), .data$taxon) |>
    dplyr::slice_head(n = as.integer(n_top))
  selected <- selected |>
    dplyr::filter(.data$taxon %in% leaders$taxon)
  grouped <- selected |>
    dplyr::group_by(.data$taxon, .data$cell_id,
                    .data$cell_lon_min, .data$cell_lat_min) |>
    dplyr::summarise(
      records = sum(.data$records_cell),
      years = dplyr::n_distinct(.data$year_analysis),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      cell_lon = .data$cell_lon_min + grid_res / 2,
      cell_lat = .data$cell_lat_min + grid_res / 2,
      taxon = factor(.data$taxon, levels = leaders$taxon)
    )
  grouped
}

# -----------------------------------------------------------------------------
# obis_add_coastline()
# Objetivo: Superponer costa mundial solo cuando están instalados sf y
#           rnaturalearth. Sin esos paquetes, entrega igualmente el mapa.
# Entradas: objeto ggplot y parámetros del dominio.
# Devuelve: objeto ggplot con/sin línea de costa.
# -----------------------------------------------------------------------------
obis_add_coastline <- function(plot, p) {
  if (requireNamespace("sf", quietly = TRUE) &&
      requireNamespace("rnaturalearth", quietly = TRUE)) {
    land <- tryCatch(
      rnaturalearth::ne_countries(scale = "medium", returnclass = "sf"),
      error = function(e) NULL
    )
    if (!is.null(land)) {
      plot <- plot + ggplot2::geom_sf(
        data = land, inherit.aes = FALSE, fill = NA,
        colour = "grey25", linewidth = 0.22
      )
    }
  }
  # sf is optional. Never reference sf:: when the package is unavailable.
  if (!requireNamespace("sf", quietly = TRUE)) {
    return(plot + ggplot2::coord_fixed(
      xlim = c(p$lon_min, p$lon_max),
      ylim = c(p$lat_min, p$lat_max), expand = FALSE
    ))
  }
  plot + ggplot2::coord_sf(
    xlim = c(p$lon_min, p$lon_max),
    ylim = c(p$lat_min, p$lat_max),
    expand = FALSE, default_crs = sf::st_crs(4326)
  )
}

# -----------------------------------------------------------------------------
# obis_plot_map_taxa()
# Objetivo: Mapear hasta n_top taxones (ordenados por total de registros),
#           mostrando registros o número de años documentados por celda.
# Entradas: cell_counts, nivel, resolución, dominio, métrica, número de taxones.
# Devuelve: ggplot faceteado por taxón, con theme_cowplot().
# Nota: una celda en blanco es "sin registros", NO una ausencia biológica.
# -----------------------------------------------------------------------------
obis_plot_map_taxa <- function(cell_counts, rank_value, grid_res, p,
                               metric = c("records", "years"), n_top = 6L) {
  metric <- match.arg(metric)
  d <- obis_map_tiles(cell_counts, rank_value, grid_res,
                      mode = metric, n_top = n_top)
  if (is.null(d) || !nrow(d)) return(NULL)
  value <- if (metric == "records") "records" else "years"
  title <- if (metric == "records") "Ocurrencias acumuladas" else "Años con registros"
  gp <- ggplot2::ggplot(d, ggplot2::aes(
    x = .data$cell_lon, y = .data$cell_lat, fill = .data[[value]]
  )) +
    ggplot2::geom_tile(width = grid_res, height = grid_res) +
    ggplot2::facet_wrap(~ taxon, ncol = 3) +
    ggplot2::scale_fill_viridis_c(
      trans = if (metric == "records") "log1p" else "identity",
      option = "D", name = title
    ) +
    ggplot2::labs(
      title = paste0("OBIS: ", title, " por celda — ", rank_value),
      subtitle = "Facetas ordenadas por volumen documental; blanco = sin registros",
      x = "Longitud (°)", y = "Latitud (°)"
    ) +
    cowplot::theme_cowplot(font_size = 10) +
    ggplot2::theme(strip.text = ggplot2::element_text(face = "bold"))
  # geom_sf puede superponerse a un mapa rectangular; el CRS geográfico
  # se fija solo cuando sf está disponible, para evitar dependencias rígidas.
  if (requireNamespace("sf", quietly = TRUE)) {
    gp <- obis_add_coastline(gp, p)
  } else {
    gp <- gp + ggplot2::coord_fixed(
      xlim = c(p$lon_min, p$lon_max),
      ylim = c(p$lat_min, p$lat_max), expand = FALSE
    )
  }
  gp
}

# -----------------------------------------------------------------------------
# obis_plot_map_years()
# Objetivo: Comparar mapas de registros por celda, cruzando taxón y año.
# Entradas: tabla de celdas, nivel taxonómico, dominio, grilla, años y n_top.
# Devuelve: ggplot facet_grid(taxon ~ year), con años espaciados disponibles.
# Nota: los años vacíos no se interpolan ni se convierten en cero ecológico.
# -----------------------------------------------------------------------------
obis_plot_map_years <- function(cell_counts, rank_value, grid_res, p,
                                selected_years = NULL, n_top = 4L,
                                metric = c("records_cell", "species_cell")) {
  metric <- match.arg(metric)
  d <- cell_counts |>
    dplyr::filter(.data$rank == .env$rank_value)
  if (!nrow(d)) return(NULL)
  leaders <- d |>
    dplyr::group_by(.data$taxon) |>
    dplyr::summarise(total = sum(.data$records_cell), .groups = "drop") |>
    dplyr::arrange(dplyr::desc(.data$total), .data$taxon) |>
    dplyr::slice_head(n = as.integer(n_top))
  if (is.null(selected_years)) {
    selected_years <- obis_selected_years(d$year_analysis, 4L)
  }
  d <- d |>
    dplyr::filter(.data$taxon %in% leaders$taxon,
                  .data$year_analysis %in% .env$selected_years) |>
    dplyr::mutate(
      taxon = factor(.data$taxon, levels = leaders$taxon),
      year_analysis = factor(.data$year_analysis, levels = selected_years),
      cell_lon = .data$cell_lon_min + grid_res / 2,
      cell_lat = .data$cell_lat_min + grid_res / 2
    )
  if (!nrow(d)) return(NULL)
  gp <- ggplot2::ggplot(d, ggplot2::aes(
    .data$cell_lon, .data$cell_lat, fill = .data[[metric]]
  )) +
    ggplot2::geom_tile(width = grid_res, height = grid_res) +
    ggplot2::facet_grid(taxon ~ year_analysis, drop = FALSE) +
    ggplot2::scale_fill_viridis_c(
      trans = if (metric == "records_cell") "log1p" else "identity",
      name = if (metric == "records_cell") "Registros" else "Especies registradas"
    ) +
    ggplot2::labs(
      title = paste0("OBIS: ", if (metric == "records_cell") "ocurrencias" else "riqueza registrada", " por celda y año — ", rank_value),
      subtitle = "Años seleccionados de forma reproducible; blanco = sin registros",
      x = "Longitud (°)", y = "Latitud (°)"
    ) +
    cowplot::theme_cowplot(font_size = 9) +
    ggplot2::theme(strip.text = ggplot2::element_text(face = "bold"))
  if (requireNamespace("sf", quietly = TRUE)) {
    gp <- obis_add_coastline(gp, p)
  } else {
    gp <- gp + ggplot2::coord_fixed(
      xlim = c(p$lon_min, p$lon_max),
      ylim = c(p$lat_min, p$lat_max), expand = FALSE
    )
  }
  gp
}

# -----------------------------------------------------------------------------
# obis_cell_accumulation()
# Objetivo: Medir la sensibilidad de riqueza REGISTRADA al número de celdas
#           con datos. Usa permutaciones aleatorias sin reemplazo de celdas.
# Entradas: clean, grupo/rango/año, grilla, dominio, n_perm y umbral target.
# Devuelve: curva promedio, umbral de celdas para recuperar una fracción
#           target de especies YA documentadas, y metadatos de cobertura.
# NOTA CRÍTICA: no es completitud de inventario, rarefacción individual ni
#              riqueza biológica verdadera; celdas sin registros se desconocen.
# -----------------------------------------------------------------------------
obis_cell_accumulation <- function(clean, p, grid_res, rank_value, taxon_value,
                                   year_value, n_perm = 50L, target = 0.90,
                                   n_steps = 35L, seed = 2026L) {
  if (!rank_value %in% names(clean)) stop("Rango no presente: ", rank_value)
  d <- clean |>
    dplyr::filter(.data$year_analysis == .env$year_value,
                  .data[[rank_value]] == .env$taxon_value,
                  !is.na(.data$species_accepted),
                  .data$species_accepted != "")
  if (!nrow(d)) return(NULL)
  ix <- pmin(ceiling((p$lon_max - p$lon_min) / grid_res) - 1L,
             floor((d$decimalLongitude - p$lon_min) / grid_res))
  iy <- pmin(ceiling((p$lat_max - p$lat_min) / grid_res) - 1L,
             floor((d$decimalLatitude - p$lat_min) / grid_res))
  d$cell_key <- paste(ix, iy, sep = "_")
  cells <- split(as.character(d$species_accepted), d$cell_key)
  cells <- lapply(cells, unique)
  n_cells <- length(cells)
  n_species <- length(unique(unlist(cells, use.names = FALSE)))
  if (n_cells < 5L || n_species < 3L) return(NULL)
  k <- sort(unique(as.integer(round(seq(1, n_cells, length.out = min(n_steps, n_cells))))))
  set.seed(seed + as.integer(year_value))
  z <- matrix(NA_real_, nrow = as.integer(n_perm), ncol = length(k))
  for (j in seq_len(n_perm)) {
    order_cells <- sample.int(n_cells, replace = FALSE)
    observed <- character()
    next_k <- 1L
    for (i in seq_len(n_cells)) {
      observed <- union(observed, cells[[order_cells[i]]])
      if (i == k[next_k]) {
        z[j, next_k] <- length(observed)
        next_k <- next_k + 1L
        if (next_k > length(k)) break
      }
    }
  }
  curve <- tibble::tibble(
    rank = rank_value, taxon = taxon_value, year = year_value,
    cells = k, observed_species_reference = n_species,
    richness_mean = colMeans(z),
    richness_q025 = apply(z, 2, stats::quantile, probs = 0.025),
    richness_q975 = apply(z, 2, stats::quantile, probs = 0.975),
    fraction_observed_species = colMeans(z) / n_species
  )
  enough <- which(curve$fraction_observed_species >= target)
  min_cells <- if (length(enough)) curve$cells[min(enough)] else NA_integer_
  summary <- tibble::tibble(
    rank = rank_value, taxon = taxon_value, year = year_value,
    n_cells = n_cells, n_records = nrow(d), n_species = n_species,
    threshold_fraction = target, min_cells_to_recover_fraction = min_cells,
    percent_observed_cells_required = 100 * min_cells / n_cells,
    diagnostic = "RECUPERACION_DE_RIQUEZA_OBSERVADA_NO_COMPLETITUD"
  )
  list(curve = curve, summary = summary)
}

# -----------------------------------------------------------------------------
# obis_cell_accumulation_screening()
# Objetivo: Aplicar el diagnóstico anterior a taxones priorizados (clase/orden)
#           y a años distribuidos a lo largo de la serie, con límite de cómputo.
# Entradas: clean, suitability, p, grid_res, años y límites computacionales.
# Devuelve: curvas y métricas comparables entre candidatos, sin inferir N mínimo
#           universal de registros por año.
# -----------------------------------------------------------------------------
obis_cell_accumulation_screening <- function(clean, suitability, p, grid_res,
                                              ranks = c("class", "order"),
                                              n_groups_per_rank = 3L,
                                              n_years = 4L, n_perm = 50L,
                                              target = 0.90) {
  if (is.null(clean) || !nrow(clean) || is.null(suitability)) return(NULL)
  candidates <- suitability |>
    dplyr::filter(.data$rank %in% .env$ranks) |>
    dplyr::arrange(dplyr::desc(.data$joint_screen),
                   dplyr::desc(.data$years_present),
                   dplyr::desc(.data$records)) |>
    dplyr::group_by(.data$rank) |>
    dplyr::slice_head(n = as.integer(n_groups_per_rank)) |>
    dplyr::ungroup()
  curves <- list(); summaries <- list(); k <- 0L
  for (i in seq_len(nrow(candidates))) {
    rank_value <- candidates$rank[i]
    taxon_value <- candidates$group[i]
    available <- clean$year_analysis[
      !is.na(clean[[rank_value]]) & clean[[rank_value]] == taxon_value
    ]
    picked <- obis_selected_years(available, n_years)
    for (yr in picked) {
      out <- obis_cell_accumulation(
        clean, p, grid_res, rank_value, taxon_value, yr,
        n_perm = n_perm, target = target
      )
      if (is.null(out)) next
      k <- k + 1L
      curves[[k]] <- out$curve
      summaries[[k]] <- out$summary
    }
  }
  if (!length(curves)) return(list(curves = tibble::tibble(),
                                   summary = tibble::tibble()))
  list(curves = dplyr::bind_rows(curves),
       summary = dplyr::bind_rows(summaries))
}

# -----------------------------------------------------------------------------
# obis_event_coverage_by_year()
# Objetivo: Estimar, SOLO para eventos independientes comparables, el número
#           mínimo interpolado de eventos que alcanza cobertura muestral meta.
# Entradas: clean, suitability, grupos/años límite, cobertura objetivo.
# Salida: tabla por grupo-año-dataset; valores NA si no se identifica suficiente
#         información o si el nivel de cobertura no puede alcanzarse.
# Fundamento: iNEXT por incidencia; NO interpreta ocurrencias como individuos.
# Precaución: requiere revisar si los eventID son verdaderas unidades de muestreo
#             bajo un protocolo comparable. Se escoge un solo dataset por año.
# -----------------------------------------------------------------------------
obis_event_coverage_by_year <- function(clean, suitability,
                                        ranks = c("class", "order"),
                                        n_groups_per_rank = 3L,
                                        n_years = 12L,
                                        target_coverage = 0.90) {
  if (!requireNamespace("iNEXT", quietly = TRUE)) {
    warning("Instala iNEXT para estimar cobertura muestral por eventos")
    return(tibble::tibble())
  }
  if (is.null(clean) || !nrow(clean) || is.null(suitability)) {
    return(tibble::tibble())
  }
  chosen <- suitability |>
    dplyr::filter(.data$rank %in% .env$ranks) |>
    dplyr::arrange(dplyr::desc(.data$joint_screen),
                   dplyr::desc(.data$years_present),
                   dplyr::desc(.data$records)) |>
    dplyr::group_by(.data$rank) |>
    dplyr::slice_head(n = as.integer(n_groups_per_rank)) |>
    dplyr::ungroup()
  rows <- list(); i <- 0L
  for (k in seq_len(nrow(chosen))) {
    level <- chosen$rank[k]; tx <- chosen$group[k]
    d <- clean |>
      dplyr::filter(.data[[level]] == .env$tx) |>
      dplyr::mutate(
        event = as.character(.data$eventID),
        ds = dplyr::coalesce(
          dplyr::na_if(as.character(.data$dataset_id), ""),
          dplyr::na_if(as.character(.data$datasetID), "")
        ),
        sp = as.character(.data$species_accepted)
      ) |>
      dplyr::filter(!is.na(.data$event), .data$event != "",
                    !is.na(.data$ds), .data$ds != "",
                    !is.na(.data$sp), .data$sp != "")
    if (!nrow(d)) next
    selected_years <- obis_selected_years(d$year_analysis, n_years)
    for (yr in selected_years) {
      dy <- d |>
        dplyr::filter(.data$year_analysis == .env$yr)
      if (!nrow(dy)) next
      ds_best <- dy |>
        dplyr::distinct(.data$ds, .data$event) |>
        dplyr::count(.data$ds, name = "events") |>
        dplyr::arrange(dplyr::desc(.data$events), .data$ds)
      dataset <- ds_best$ds[1]
      dy <- dy |>
        dplyr::filter(.data$ds == .env$dataset) |>
        dplyr::distinct(.data$event, .data$sp)
      n_ev <- dplyr::n_distinct(dy$event)
      n_sp <- dplyr::n_distinct(dy$sp)
      status <- "EVENTOS_O_ESPECIES_INSUFICIENTES"
      observed_coverage <- NA_real_
      min_events <- NA_integer_
      if (n_ev >= 10 && n_sp >= 3 && n_ev * n_sp <= 1e6) {
        m <- stats::xtabs(~ sp + event, data = dy)
        m <- 1L * (as.matrix(m) > 0)
        fit <- tryCatch(iNEXT::iNEXT(
          m, q = 0, datatype = "incidence_raw",
          size = sort(unique(c(seq_len(min(n_ev, 100L)),
                               round(seq(1, n_ev, length.out = 100))))),
          nboot = 0
        ), error = identity)
        if (inherits(fit, "error")) {
          status <- paste0("ERROR_INEXT: ", conditionMessage(fit))
        } else {
          t <- fit$iNextEst$size_based
          # Para iNEXT 3.x: columna SC = sample coverage de cada tamaño m.
          t <- t[t$Order.q == 0 & t$m <= n_ev, , drop = FALSE]
          observed_coverage <- if (nrow(t)) max(t$SC, na.rm = TRUE) else NA_real_
          eligible <- t$m[is.finite(t$SC) & t$SC >= target_coverage]
          if (length(eligible)) {
            min_events <- as.integer(min(eligible))
            status <- "COBERTURA_META_ALCANZADA"
          } else {
            status <- "COBERTURA_META_NO_ALCANZADA"
          }
        }
      }
      i <- i + 1L
      rows[[i]] <- tibble::tibble(
        rank = level, taxon = tx, year = yr, dataset = dataset,
        events = n_ev, species = n_sp,
        coverage_target = target_coverage,
        coverage_at_observed_size = observed_coverage,
        min_events_target = min_events, status = status
      )
    }
  }
  if (!length(rows)) return(tibble::tibble())
  dplyr::bind_rows(rows)
}

# -----------------------------------------------------------------------------
# obis_plot_cell_accumulation()
# Objetivo: Graficar recuperación de especies previamente observadas al aumentar
#           el número de celdas informadas, con variabilidad entre permutaciones.
# Entradas: resultado$curves y rank_value.
# Devuelve: ggplot facetado taxón × año con theme_cowplot().
# -----------------------------------------------------------------------------
obis_plot_cell_accumulation <- function(curves, rank_value) {
  d <- curves |> dplyr::filter(.data$rank == .env$rank_value)
  if (!nrow(d)) return(NULL)
  ggplot2::ggplot(d, ggplot2::aes(
    .data$cells, .data$fraction_observed_species
  )) +
    ggplot2::geom_ribbon(ggplot2::aes(
      ymin = .data$richness_q025 / .data$observed_species_reference,
      ymax = .data$richness_q975 / .data$observed_species_reference
    ), fill = "grey75", alpha = 0.65) +
    ggplot2::geom_line(linewidth = 0.7, colour = "black") +
    ggplot2::geom_hline(yintercept = 0.90, linewidth = 0.4,
                        linetype = "dashed", colour = "grey40") +
    ggplot2::facet_grid(taxon ~ year, scales = "free_x") +
    ggplot2::scale_y_continuous(limits = c(0, 1), labels = scales::label_percent()) +
    ggplot2::labs(
      title = paste("OBIS: sensibilidad de riqueza documentada a la cobertura espacial —", rank_value),
      subtitle = "Permutaciones de celdas ocupadas; referencia = riqueza observada, no riqueza verdadera",
      x = "Número de celdas con registros acumuladas",
      y = "Fracción de especies registradas recuperadas"
    ) +
    cowplot::theme_cowplot(font_size = 10)
}

# -----------------------------------------------------------------------------
# obis_record_thinning()
# Objetivo: Evaluar si el MAPA DE RIQUEZA REGISTRADA por celda permanece
#           parecido al retirar aleatoriamente un porcentaje de ocurrencias.
# Entradas: clean, grupo y año, dominio, resolución, n_perm, umbrales.
# Devuelve: curvas de estabilidad y n mínimo *condicional al conjunto observado*.
# Criterios de sensibilidad (exploratorios): correlación Spearman con mapa de
# referencia >= rho_target, >= species_target de especies registradas y >=
# cells_target de celdas con registros. Exige que >= 90 % de permutaciones
# satisfaga simultáneamente las tres condiciones.
# NO es un mínimo biológico ni recomienda muestrear N ocurrencias: no corrige
# selectividad ni áreas sin datos, y el mapa de referencia también es sesgado.
# -----------------------------------------------------------------------------
obis_record_thinning <- function(clean, p, grid_res, rank_value, taxon_value,
                                 year_value, n_perm = 30L,
                                 rho_target = 0.90, species_target = 0.90,
                                 cells_target = 0.80, pass_fraction = 0.90,
                                 n_steps = 15L, seed = 2026L) {
  d <- clean |>
    dplyr::filter(.data$year_analysis == .env$year_value,
                  .data[[rank_value]] == .env$taxon_value,
                  !is.na(.data$species_accepted),
                  .data$species_accepted != "")
  n <- nrow(d)
  if (n < 30L) return(NULL)
  nx <- ceiling((p$lon_max - p$lon_min) / grid_res)
  ny <- ceiling((p$lat_max - p$lat_min) / grid_res)
  ix <- pmin(nx - 1L, pmax(0L,
                 floor((d$decimalLongitude - p$lon_min) / grid_res)))
  iy <- pmin(ny - 1L, pmax(0L,
                 floor((d$decimalLatitude - p$lat_min) / grid_res)))
  cell <- paste(ix, iy, sep = "_")
  reference <- tapply(d$species_accepted, cell,
                       function(z) length(unique(z)))
  ref_species <- length(unique(d$species_accepted))
  if (length(reference) < 5L || ref_species < 5L ||
      length(unique(reference)) < 2L) return(NULL)
  sizes <- sort(unique(pmax(10L, as.integer(round(n *
    seq(0.1, 1, length.out = as.integer(n_steps)))))))
  set.seed(as.integer(seed + year_value))
  lines <- vector("list", length(sizes))
  for (j in seq_along(sizes)) {
    k <- sizes[j]
    sims <- vector("list", as.integer(n_perm))
    for (b in seq_len(n_perm)) {
      indices <- if (k == n) seq_len(n) else sample.int(n, k)
      subspecies <- tapply(d$species_accepted[indices], cell[indices],
                           function(z) length(unique(z)))
      aligned <- stats::setNames(rep(0, length(reference)), names(reference))
      aligned[names(subspecies)] <- subspecies
      rho <- suppressWarnings(stats::cor(
        as.numeric(reference), as.numeric(aligned),
        method = "spearman"
      ))
      recovered_species <- length(unique(d$species_accepted[indices])) / ref_species
      retained_cells <- length(subspecies) / length(reference)
      passed <- is.finite(rho) && rho >= rho_target &&
        recovered_species >= species_target &&
        retained_cells >= cells_target
      sims[[b]] <- tibble::tibble(
        rho = rho, frac_species = recovered_species,
        frac_cells = retained_cells, passed = passed
      )
    }
    s <- dplyr::bind_rows(sims)
    lines[[j]] <- tibble::tibble(
      rank = rank_value, taxon = taxon_value, year = year_value,
      n_records = n, sample_records = k, sampled_fraction = k / n,
      median_rho = stats::median(s$rho, na.rm = TRUE),
      mean_fraction_species = mean(s$frac_species),
      mean_fraction_cells = mean(s$frac_cells),
      pass_fraction = mean(s$passed)
    )
  }
  curve <- dplyr::bind_rows(lines)
  possible <- curve$sample_records[curve$pass_fraction >= pass_fraction]
  minimum <- if (length(possible)) min(possible) else NA_integer_
  summary <- tibble::tibble(
    rank = rank_value, taxon = taxon_value, year = year_value,
    observed_records = n, observed_species = ref_species,
    observed_cells = length(reference),
    min_records_map_stability_conditional = minimum,
    fraction_records_needed = minimum / n,
    rho_target = rho_target,
    species_target = species_target,
    cells_target = cells_target,
    required_success_fraction = pass_fraction,
    status = if (is.na(minimum)) "SIN_ESTABILIDAD_DETECTABLE" else
      "ESTABILIDAD_INTERNA_OBSERVADA_NO_VALIDEZ_ECOLOGICA"
  )
  list(curve = curve, summary = summary)
}

# -----------------------------------------------------------------------------
# obis_record_thinning_screening()
# Objetivo: aplicar el diagnóstico anterior a taxones prioritarios y años
#           distribuidos a lo largo del periodo, con costos acotados.
# Entradas: base, screening integrado, dominio, resolución y límites.
# Devuelve: curvas y tabla de número mínimo CONDICIONAL de registros/año.
# -----------------------------------------------------------------------------
obis_record_thinning_screening <- function(clean, suitability, p, grid_res,
                                           ranks = c("class", "order"),
                                           n_groups_per_rank = 2L,
                                           n_years = 4L, n_perm = 30L) {
  if (is.null(clean) || !nrow(clean) || is.null(suitability)) return(NULL)
  chosen <- suitability |>
    dplyr::filter(.data$rank %in% .env$ranks) |>
    dplyr::arrange(dplyr::desc(.data$joint_screen),
                   dplyr::desc(.data$years_present),
                   dplyr::desc(.data$records)) |>
    dplyr::group_by(.data$rank) |>
    dplyr::slice_head(n = as.integer(n_groups_per_rank)) |>
    dplyr::ungroup()
  curves <- list(); summaries <- list(); i <- 0L
  for (k in seq_len(nrow(chosen))) {
    rank_value <- chosen$rank[k]
    taxon_value <- chosen$group[k]
    present <- clean$year_analysis[
      !is.na(clean[[rank_value]]) & clean[[rank_value]] == taxon_value
    ]
    for (yr in obis_selected_years(present, n_years)) {
      x <- obis_record_thinning(
        clean, p, grid_res, rank_value, taxon_value, yr,
        n_perm = n_perm
      )
      if (is.null(x)) next
      i <- i + 1L
      curves[[i]] <- x$curve
      summaries[[i]] <- x$summary
    }
  }
  if (!length(curves)) return(list(curves = tibble::tibble(),
                                   summary = tibble::tibble()))
  list(curves = dplyr::bind_rows(curves),
       summary = dplyr::bind_rows(summaries))
}

# -----------------------------------------------------------------------------
# obis_plot_record_thinning()
# Objetivo: mostrar estabilidad relativa del mapa de riqueza documentada al
#           aumentar el número de registros re-muestreados.
# Entradas: curvas, nivel taxonómico.
# Devuelve: gráfico facetado con tema cowplot. La línea 0.9 es indicativa.
# -----------------------------------------------------------------------------
obis_plot_record_thinning <- function(curves, rank_value) {
  d <- curves |> dplyr::filter(.data$rank == .env$rank_value)
  if (!nrow(d)) return(NULL)
  ggplot2::ggplot(d, ggplot2::aes(
    .data$sample_records, .data$pass_fraction
  )) +
    ggplot2::geom_line(linewidth = 0.75, colour = "black") +
    ggplot2::geom_point(size = 1, colour = "black") +
    ggplot2::geom_hline(yintercept = 0.9, linetype = "dashed",
                        colour = "grey50") +
    ggplot2::facet_grid(taxon ~ year, scales = "free_x") +
    ggplot2::scale_y_continuous(limits = c(0, 1),
                                labels = scales::label_percent()) +
    ggplot2::labs(
      title = paste("OBIS: estabilidad interna del mapa de riqueza registrada —", rank_value),
      subtitle = "Éxito conjunto: correlación espacial, riqueza recuperada y celdas retenidas",
      x = "Número de ocurrencias re-muestreadas",
      y = "Porcentaje de remuestreos que cumplen los criterios"
    ) +
    cowplot::theme_cowplot(font_size = 10)
}


# =============================================================================
# EXTENSIÓN: SATURACIÓN DE RIQUEZA DOCUMENTADA EN FUNCIÓN DE REGISTROS OBIS
# =============================================================================
# Los 'registros' son filas de ocurrencia y NO individuos independientes.
# El submuestreo sin reemplazo calcula riqueza esperada condicional al inventario
# observado; su aparente meseta NO prueba completitud biológica ni representatividad.

# -----------------------------------------------------------------------------
# obis_rarefaction_records()
# Objetivo: curva analítica exacta de riqueza esperada al submuestrear n registros.
# Entradas: vector de especies (una etiqueta por registro), target (0-1), puntos.
# Salidas: curva con riqueza esperada y proporción recuperada; resumen n90/n95.
# Método: E[S(n)] = sum_i {1 - choose(N-f_i,n)/choose(N,n)}.
# Se usa lchoose() para evitar desbordamientos numéricos.
# -----------------------------------------------------------------------------
obis_rarefaction_records <- function(species, targets = c(0.90, 0.95),
                                     n_points = 35L) {
  species <- trimws(as.character(species))
  species <- species[!is.na(species) & nzchar(species)]
  N <- length(species)
  if (!N) return(NULL)
  freq <- as.numeric(table(species))
  S <- length(freq)
  # Curva analítica exacta para N <= 500; búsqueda binaria para N mayor.
  richness_at <- function(n) {
    if (n <= 0L) return(0)
    if (n >= N) return(as.numeric(S))
    p_not_seen <- numeric(length(freq))
    eligible <- n <= (N - freq)
    p_not_seen[eligible] <- exp(lchoose(N - freq[eligible], n) - lchoose(N, n))
    sum(1 - p_not_seen)
  }
  grid <- sort(unique(as.integer(c(
    1L,
    N,
    round(seq(1, N, length.out = max(2L, as.integer(n_points))))
  ))))
  values <- vapply(grid, richness_at, numeric(1))
  curve <- tibble::tibble(
    sample_records = grid, richness_expected = values,
    observed_species = S, recovered_fraction = values / S
  )
  find_target <- function(t) {
    if (N < 2L || S < 2L) return(NA_integer_)
    low <- 1L
    high <- as.integer(N)
    while (low < high) {
      mid <- low + (high - low) %/% 2L
      if (richness_at(mid) / S >= t) high <- mid else low <- mid + 1L
    }
    as.integer(low)
  }
  target_sizes <- vapply(targets, find_target, integer(1))
  names(target_sizes) <- paste0('n', as.integer(round(targets * 100)))
  list(
    curve = curve,
    summary = tibble::tibble(
      n_records = N, n_species = S,
      n_required_90 = unname(target_sizes[['n90']]),
      n_required_95 = unname(target_sizes[['n95']]),
      fraction_n95 = unname(target_sizes[['n95']]) / N,
      n_singletons = sum(freq == 1L),
      singleton_fraction_species = sum(freq == 1L) / S,
      n_distinct_species = S,
      status = if (S < 2L) 'UNA_ESPECIE_NO_EVALUABLE' else
        if (N < 20L) 'MUESTRA_ESCASA_CAUTELA' else
        'ESTABILIDAD_CONDICIONAL_AL_INVENTARIO'
    )
  )
}

# -----------------------------------------------------------------------------
# obis_records_saturation_screening()
# Objetivo: evaluar riqueza vs número de ocurrencias por grupo-año (dominio)
#           y por grupo-año-celda con resolución grid_res.
# Entradas: clean, tabla suitability, p, resolución, rangos, número de grupos,
#           mínimo descriptivo por celda y máximo de celdas diagnosticadas.
# Salidas: domain_curves, domain_summary, cell_curves, cell_summary, audit.
# Notas: TODOS los años del periodo con registros se analizan a nivel dominio;
#        las celdas con 1-19 registros se auditan como insuficientes y NO se
#        confunden con celdas ausentes. Los umbrales son 90/95% de S_obs,
#        no del total desconocido de especies presentes.
# -----------------------------------------------------------------------------
obis_records_saturation_screening <- function(
    clean, suitability, p, grid_res = 0.25,
    ranks = c('phylum', 'class', 'order'), n_groups_per_rank = 3L,
    min_cell_records = 20L, max_cell_units = 10000L,
    n_points = 35L) {
  if (is.null(clean) || !nrow(clean) || is.null(suitability) ||
      !nrow(suitability)) return(NULL)
  if (!is.numeric(grid_res) || length(grid_res) != 1L ||
      !is.finite(grid_res) || grid_res <= 0) stop('grid_res inválido')
  ranks <- intersect(ranks, names(clean))
  if (!length(ranks)) return(NULL)
  chosen <- suitability |>
    dplyr::filter(.data$rank %in% .env$ranks) |>
    dplyr::arrange(dplyr::desc(.data$joint_screen),
                   dplyr::desc(.data$years_present),
                   dplyr::desc(.data$records)) |>
    dplyr::group_by(.data$rank) |>
    dplyr::slice_head(n = as.integer(n_groups_per_rank)) |>
    dplyr::ungroup()
  if (!nrow(chosen)) return(NULL)

  nx <- ceiling((p$lon_max - p$lon_min) / grid_res)
  ny <- ceiling((p$lat_max - p$lat_min) / grid_res)
  cell_x <- pmin(nx - 1L, pmax(0L,
    floor((clean$decimalLongitude - p$lon_min) / grid_res)))
  cell_y <- pmin(ny - 1L, pmax(0L,
    floor((clean$decimalLatitude - p$lat_min) / grid_res)))
  base <- clean |>
    dplyr::mutate(cell_x = as.integer(cell_x), cell_y = as.integer(cell_y),
      cell_id = paste0('C', .data$cell_x, '_', .data$cell_y),
      cell_lon_min = p$lon_min + .data$cell_x * grid_res,
      cell_lat_min = p$lat_min + .data$cell_y * grid_res)

  dom_curves <- list(); dom_summaries <- list()
  cell_curves <- list(); cell_summaries <- list(); audits <- list()
  di <- 0L; ci <- 0L; ai <- 0L
  for (k in seq_len(nrow(chosen))) {
    nivel <- as.character(chosen$rank[[k]])
    grupo <- as.character(chosen$group[[k]])
    z <- base |>
      dplyr::filter(!is.na(.data[[nivel]]),
                    .data[[nivel]] == .env$grupo,
                    !is.na(.data$species_accepted),
                    trimws(.data$species_accepted) != '')
    if (!nrow(z)) next
    # El análisis por dominio incluye todos los años presentes.
    for (yr in sort(unique(z$year_analysis))) {
      zy <- z[z$year_analysis == yr, , drop = FALSE]
      fit <- obis_rarefaction_records(zy$species_accepted,
                                     n_points = n_points)
      if (is.null(fit)) next
      di <- di + 1L
      dom_curves[[di]] <- dplyr::mutate(fit$curve, rank = nivel,
                                       taxon = grupo, year_analysis = yr)
      dom_summaries[[di]] <- dplyr::mutate(fit$summary, rank = nivel,
                                          taxon = grupo, year_analysis = yr,
                                          n_cells_documented = dplyr::n_distinct(zy$cell_id))

      # Mantener registro de TODAS las celdas con ocurrencias.
      groups <- split(seq_len(nrow(zy)), zy$cell_id)
      for (cell_name in names(groups)) {
        idx <- groups[[cell_name]]
        u <- zy[idx, , drop = FALSE]
        cell_n <- nrow(u)
        cell_S <- dplyr::n_distinct(u$species_accepted)
        ci <- ci + 1L
        state <- if (cell_n < min_cell_records) 'MENOS_DE_MIN_REGISTROS' else
          if (cell_S < 2L) 'MENOS_DE_DOS_ESPECIES' else
          if (ci > max_cell_units) 'LIMITE_DE_CELDAS_EVALUADAS' else 'EVALUADO'
        row <- tibble::tibble(
          rank = nivel, taxon = grupo, year_analysis = yr,
          cell_id = cell_name, cell_lon_min = u$cell_lon_min[[1]],
          cell_lat_min = u$cell_lat_min[[1]],
          n_records = cell_n, n_species = cell_S,
          n_required_90 = NA_integer_, n_required_95 = NA_integer_,
          fraction_n95 = NA_real_, n_singletons = NA_integer_,
          singleton_fraction_species = NA_real_, status = state)
        if (state == 'EVALUADO') {
          fit_cell <- obis_rarefaction_records(u$species_accepted,
                                              n_points = n_points)
          if (!is.null(fit_cell)) {
            row$n_required_90 <- fit_cell$summary$n_required_90
            row$n_required_95 <- fit_cell$summary$n_required_95
            row$fraction_n95 <- fit_cell$summary$fraction_n95
            row$n_singletons <- fit_cell$summary$n_singletons
            row$singleton_fraction_species <- fit_cell$summary$singleton_fraction_species
            cell_curves[[ci]] <- dplyr::mutate(fit_cell$curve,
              rank = nivel, taxon = grupo, year_analysis = yr,
              cell_id = cell_name)
          }
        }
        cell_summaries[[ci]] <- row
      }
    }
  }
  domain_summary <- dplyr::bind_rows(dom_summaries)
  cell_summary <- dplyr::bind_rows(cell_summaries)
  if (!nrow(domain_summary)) return(NULL)
  # Cobertura de celdas evaluables y advertencia contra la circularidad del N95.
  audits <- domain_summary |>
    dplyr::left_join(
      cell_summary |>
        dplyr::group_by(.data$rank, .data$taxon, .data$year_analysis) |>
        dplyr::summarise(
          cells_with_records = dplyr::n(),
          cells_evaluable = sum(.data$status == 'EVALUADO'),
          cells_low_records = sum(.data$status == 'MENOS_DE_MIN_REGISTROS'),
          cells_single_species = sum(.data$status == 'MENOS_DE_DOS_ESPECIES'),
          cells_over_limit = sum(.data$status == 'LIMITE_DE_CELDAS_EVALUADAS'),
          .groups = 'drop'
        ),
      by = c('rank', 'taxon', 'year_analysis')
    ) |>
    dplyr::mutate(
      fraction_cells_evaluable = .data$cells_evaluable / pmax(1, .data$cells_with_records),
      n95_is_near_full_sample = !is.na(.data$fraction_n95) & .data$fraction_n95 >= 0.90
    )
  list(domain_curves = dplyr::bind_rows(dom_curves),
       domain_summary = domain_summary,
       cell_curves = dplyr::bind_rows(cell_curves),
       cell_summary = cell_summary, audit = audits,
       selected_groups = dplyr::select(chosen, dplyr::all_of(c('rank', 'group', 'records'))))
}

# -----------------------------------------------------------------------------
# obis_plot_saturation_domain()
# Objetivo: curvas por taxón con facetas por año y orden descendente de registros.
# -----------------------------------------------------------------------------
obis_plot_saturation_domain <- function(curves, rank_value, n_top = 3L,
                                        years_plot = NULL) {
  d <- curves |> dplyr::filter(.data$rank == .env$rank_value)
  if (!nrow(d)) return(NULL)
  # Orden mediante registros máximos por cada año (N observado).
  leaders <- d |>
    dplyr::group_by(.data$taxon, .data$year_analysis) |>
    dplyr::summarise(N = max(.data$sample_records), .groups = 'drop') |>
    dplyr::group_by(.data$taxon) |>
    dplyr::summarise(total = sum(.data$N), .groups = 'drop') |>
    dplyr::arrange(dplyr::desc(.data$total), .data$taxon) |>
    dplyr::slice_head(n = as.integer(n_top))
  if (is.null(years_plot)) years_plot <- obis_selected_years(d$year_analysis, 4L)
  d <- d |>
    dplyr::filter(.data$taxon %in% .env$leaders$taxon,
                  .data$year_analysis %in% .env$years_plot) |>
    dplyr::mutate(taxon = factor(.data$taxon, levels = leaders$taxon))
  if (!nrow(d)) return(NULL)
  ggplot2::ggplot(d, ggplot2::aes(.data$sample_records,
                                 .data$richness_expected)) +
    ggplot2::geom_line(linewidth = 0.75, colour = '#202020') +
    ggplot2::geom_point(size = 0.55, colour = '#202020') +
    ggplot2::facet_grid(taxon ~ year_analysis, scales = 'free') +
    ggplot2::labs(
      title = paste0('OBIS: saturación documental por registros — ', rank_value),
      subtitle = 'Curvas condicionales a las especies observadas; no estiman riqueza verdadera',
      x = 'Número de ocurrencias OBIS', y = 'Riqueza esperada de especies registradas'
    ) + cowplot::theme_cowplot(font_size = 10)
}

# -----------------------------------------------------------------------------
# obis_plot_saturation_thresholds()
# Objetivo: comparar N observado, N90 y N95 por grupo-año.
# -----------------------------------------------------------------------------
obis_plot_saturation_thresholds <- function(summary, rank_value, n_top = 8L) {
  d <- summary |> dplyr::filter(.data$rank == .env$rank_value)
  if (!nrow(d)) return(NULL)
  leaders <- d |>
    dplyr::group_by(.data$taxon) |>
    dplyr::summarise(total = sum(.data$n_records), .groups = 'drop') |>
    dplyr::arrange(dplyr::desc(.data$total), .data$taxon) |>
    dplyr::slice_head(n = as.integer(n_top))
  d <- d |>
    dplyr::filter(.data$taxon %in% .env$leaders$taxon) |>
    dplyr::mutate(taxon = factor(.data$taxon, levels = leaders$taxon))
  gp <- ggplot2::ggplot(d, ggplot2::aes(x = .data$year_analysis)) +
    ggplot2::geom_line(ggplot2::aes(y = .data$n_records, colour = 'N observado'),
                       linewidth = 0.65) +
    ggplot2::geom_line(ggplot2::aes(y = .data$n_required_90, colour = 'N90'),
                       linewidth = 0.65) +
    ggplot2::geom_line(ggplot2::aes(y = .data$n_required_95, colour = 'N95'),
                       linewidth = 0.65) +
    ggplot2::facet_wrap(~ taxon, scales = 'free_y', ncol = 3) +
    ggplot2::labs(title = paste0('OBIS: registros y umbrales de rarefacción — ',rank_value),
                  subtitle = 'N90/N95 recuperan 90/95 % de especies YA documentadas',
                  x = 'Año', y = 'Número de registros', colour = 'Serie') +
    cowplot::theme_cowplot(font_size = 10)
  gp
}

# -----------------------------------------------------------------------------
# obis_plot_saturation_map()
# Objetivo: distribución espacial de riqueza observada y proporción requerida
#           N95/N en celdas diagnosticables (blanco: sin datos evaluables).
# -----------------------------------------------------------------------------
obis_plot_saturation_map <- function(cell_summary, rank_value, p, grid_res,
                                     metric = c('n_records', 'n_species',
                                                'fraction_n95'),
                                     n_top = 3L, years_plot = NULL) {
  metric <- match.arg(metric)
  d <- cell_summary |> dplyr::filter(.data$rank == .env$rank_value)
  if (!nrow(d)) return(NULL)
  leaders <- d |>
    dplyr::group_by(.data$taxon) |>
    dplyr::summarise(total = sum(.data$n_records), .groups = 'drop') |>
    dplyr::arrange(dplyr::desc(.data$total), .data$taxon) |>
    dplyr::slice_head(n = as.integer(n_top))
  if (is.null(years_plot)) years_plot <- obis_selected_years(d$year_analysis, 4L)
  d <- d |>
    dplyr::filter(.data$taxon %in% .env$leaders$taxon,
                  .data$year_analysis %in% .env$years_plot) |>
    dplyr::mutate(taxon = factor(.data$taxon, levels = leaders$taxon),
      cell_lon = .data$cell_lon_min + grid_res / 2,
      cell_lat = .data$cell_lat_min + grid_res / 2)
  if (!nrow(d)) return(NULL)
  gp <- ggplot2::ggplot(d, ggplot2::aes(.data$cell_lon, .data$cell_lat,
                                        fill = .data[[metric]])) +
    ggplot2::geom_tile(width = grid_res, height = grid_res) +
    ggplot2::facet_grid(taxon ~ year_analysis) +
    ggplot2::scale_fill_viridis_c(
      trans = if (metric == 'n_records') 'log1p' else 'identity',
      name = switch(metric, n_records = 'Registros', n_species = 'Especies',
                    fraction_n95 = 'N95 / N')) +
    ggplot2::labs(title = paste0('OBIS: ', metric, ' por celda y año — ', rank_value),
      subtitle = 'Blanco = sin registros o sin estimación; N95 es condicional al inventario',
      x = 'Longitud (°)', y = 'Latitud (°)') +
    cowplot::theme_cowplot(font_size = 9) +
    ggplot2::theme(strip.text = ggplot2::element_text(face = 'bold'))
  if (requireNamespace('sf', quietly = TRUE)) {
    obis_add_coastline(gp, p)
  } else {
    gp + ggplot2::coord_fixed(xlim = c(p$lon_min, p$lon_max),
                               ylim = c(p$lat_min, p$lat_max), expand = FALSE)
  }
}

# -----------------------------------------------------------------------------
# obis_assert_functions()
# Objetivo: Detectar versiones incompletas de main.R antes de procesar OBIS.
# Entradas: names_expected, nombres exactos de funciones usadas por el QMD.
# Devuelve: TRUE invisible, o error con la lista de funciones no definidas.
# No ejecuta descargas ni transforma la base de datos.
# -----------------------------------------------------------------------------
obis_assert_functions <- function(names_expected, env = parent.frame()) {
  missing <- names_expected[!vapply(names_expected, function(nm) {
    exists(nm, envir = env, mode = "function", inherits = TRUE)
  }, logical(1))]
  if (length(missing)) {
    stop("main.R incompleto: faltan funciones: ",
         paste(missing, collapse = ", "),
         ". Asegura que el QMD carga el archivo actualizado en R/main.R.",
         call. = FALSE)
  }
  invisible(TRUE)
}
