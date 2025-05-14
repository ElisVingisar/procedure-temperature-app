# --- extract_procedures.R ---

library(DBI)

# Fetch daily counts of a single procedure by care site name
get_procedures <- function(conn, procedure_concept_id, start_date, end_date) {
  query <- sprintf("
  SELECT 
    po.procedure_date AS date,
    cs.care_site_name,
    COUNT(*) AS procedure_count
  FROM procedure_occurrence po
  JOIN visit_occurrence vo ON po.visit_occurrence_id = vo.visit_occurrence_id
  JOIN care_site cs ON vo.care_site_id = cs.care_site_id
  WHERE po.procedure_concept_id = %d
    AND po.procedure_date BETWEEN '%s' AND '%s'
    AND cs.care_site_name IN (
      'Tartu Ülikooli Kliinikum SA',
      'Ida-Tallinna Keskhaigla AS',
      'Põhja-Eesti Regionaalhaigla SA',
      'Lääne-Tallinna Keskhaigla AS',
      'Tallinna Lastehaigla SA',
      'Pärnu Haigla SA',
      'Läänemaa Haigla SA',
      'Hiiumaa Haigla SA',
      'Ida-Viru Keskhaigla SA',
      'Rakvere Haigla AS',
      'Kuressaare Haigla SA',
      'Narva Haigla SA',
      'Järvamaa Haigla AS',
      'Valga Haigla AS',
      'Viljandi Haigla SA',
      'Lõuna-Eesti Haigla AS',
      'Jõgeva Haigla SA',
      'Raplamaa Haigla SA',
      'Rapla Maakonnahaigla SA'
    )
  GROUP BY po.procedure_date, cs.care_site_name
  ORDER BY po.procedure_date;
", procedure_concept_id, start_date, end_date)

  
  result <- dbGetQuery(conn, query)

  return(result)
}

# Fetch daily counts for multiple procedures by care site
get_multiple_procedures <- function(conn, procedure_ids, start_date, end_date) {
  if (length(procedure_ids) == 0) return(data.frame())
  
  proc_string <- paste(procedure_ids, collapse = ", ")
  
  query <- sprintf("
    SELECT
      po.procedure_concept_id,
      po.procedure_date AS date,
      cs.care_site_name,
      COUNT(*) AS procedure_count
    FROM procedure_occurrence po
    JOIN visit_occurrence vo ON po.visit_occurrence_id = vo.visit_occurrence_id
    JOIN care_site cs ON vo.care_site_id = cs.care_site_id
    WHERE po.procedure_concept_id IN (%s)
      AND po.procedure_date BETWEEN '%s' AND '%s'
      AND cs.care_site_name IN (
        'Tartu Ülikooli Kliinikum SA',
        'Ida-Tallinna Keskhaigla AS',
        'Põhja-Eesti Regionaalhaigla SA',
        'Lääne-Tallinna Keskhaigla AS',
        'Tallinna Lastehaigla SA',
        'Pärnu Haigla SA',
        'Läänemaa Haigla SA',
        'Hiiumaa Haigla SA',
        'Ida-Viru Keskhaigla SA',
        'Rakvere Haigla AS',
        'Kuressaare Haigla SA',
        'Narva Haigla SA',
        'Järvamaa Haigla AS',
        'Valga Haigla AS',
        'Viljandi Haigla SA',
        'Lõuna-Eesti Haigla AS',
        'Jõgeva Haigla SA',
        'Raplamaa Haigla SA',
        'Rapla Maakonnahaigla SA'
        )
    GROUP BY po.procedure_concept_id, po.procedure_date, cs.care_site_name
    ORDER BY po.procedure_date;
  ", proc_string, start_date, end_date)
  
  result <- dbGetQuery(conn, query)
  return(result)
}

# Load preselected procedures from CSV file
get_selected_procedures <- function() {
  filepath <- "../queries/selected_procedures.csv"
  df <- read.csv(filepath)
  df <- df[df$procedure_concept_id != 0, ] #Exclude empty or missing concept IDs
  return(df)
}
