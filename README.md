# CAUSALIMPACT – BRAZILIAN REGIONS
# UNIVARIATE vs MULTIVARIATE MODELS

library(readxl)
library(dplyr)
library(tidyr)
library(zoo)
library(CausalImpact)
library(ggplot2)
library(patchwork)

# READ DATA

dados_wide <- read_excel(
  "C:/Users/Downloads/regioes.xlsx",
  sheet = "regioes"
)

colnames(dados_wide) <- trimws(colnames(dados_wide))

# CONVERT TO LONG FORMAT

dados_long <- dados_wide %>%
  pivot_longer(
    cols = -ano,
    names_to = "region",
    values_to = "rate"
  ) %>%
  arrange(region, ano)

# TRANSLATE REGION NAMES

dados_long <- dados_long %>%
  mutate(
    region = recode(region,
                    "Norte" = "North",
                    "Nordeste" = "Northeast",
                    "Sudeste" = "Southeast",
                    "Sul" = "South",
                    "Centro-Oeste" = "Midwest"
    )
  )

# FUNCTION FOR ANALYSIS

analise_regiao <- function(base, regiao, ano_intervencao = 2020){
  
  base <- base %>% arrange(ano)
  
  df_regiao <- base %>%
    filter(region == regiao)
  
  # UNIVARIATE MODEL
  
  serie_uni <- zoo(
    df_regiao$rate,
    order.by = df_regiao$ano
  )
  
  impact_uni <- CausalImpact(
    serie_uni,
    pre.period = c(min(df_regiao$ano), ano_intervencao - 1),
    post.period = c(ano_intervencao, max(df_regiao$ano))
  )
  
  g_uni <- data.frame(
    ano = as.numeric(time(impact_uni$series)),
    predicted = impact_uni$series$point.pred,
    lower = impact_uni$series$point.pred.lower,
    upper = impact_uni$series$point.pred.upper,
    effect = impact_uni$series$point.effect,
    eff_low = impact_uni$series$point.effect.lower,
    eff_up = impact_uni$series$point.effect.upper,
    cumul = impact_uni$series$cum.effect,
    cum_low = impact_uni$series$cum.effect.lower,
    cum_up = impact_uni$series$cum.effect.upper
  )
  
  # MULTIVARIATE MODEL
  
  controles <- base %>%
    filter(region != regiao) %>%
    pivot_wider(
      names_from = region,
      values_from = rate
    )
  
  matriz <- cbind(
    resposta = df_regiao$rate,
    controles[,-1]
  )
  
  serie_multi <- zoo(
    matriz,
    order.by = df_regiao$ano
  )
  
  impact_multi <- CausalImpact(
    serie_multi,
    pre.period = c(min(df_regiao$ano), ano_intervencao - 1),
    post.period = c(ano_intervencao, max(df_regiao$ano))
  )
  
  g_multi <- data.frame(
    ano = as.numeric(time(impact_multi$series)),
    predicted = impact_multi$series$point.pred,
    lower = impact_multi$series$point.pred.lower,
    upper = impact_multi$series$point.pred.upper,
    effect = impact_multi$series$point.effect,
    eff_low = impact_multi$series$point.effect.lower,
    eff_up = impact_multi$series$point.effect.upper,
    cumul = impact_multi$series$cum.effect,
    cum_low = impact_multi$series$cum.effect.lower,
    cum_up = impact_multi$series$cum.effect.upper
  )
  
  anos <- seq(min(g_uni$ano), max(g_uni$ano), 1)
  
    # PANEL 1 – OBSERVED VS COUNTERFACTUAL

  p1 <- ggplot() +    
    geom_ribbon(
      data=g_uni,
      aes(x=ano,ymin=lower,ymax=upper,fill="Univariate"),
      alpha=.25
    ) +    
    geom_line(
      data=g_uni,
      aes(x=ano,y=predicted,color="Univariate"),
      linewidth=1
    ) +    
    geom_ribbon(
      data=g_multi,
      aes(x=ano,ymin=lower,ymax=upper,fill="Multivariate"),
      alpha=.25
    ) +    
    geom_line(
      data=g_multi,
      aes(x=ano,y=predicted,color="Multivariate"),
      linewidth=1
    ) +    
    geom_line(
      data=df_regiao,
      aes(x=ano,y=rate,color="Observed"),
      linewidth=1
    ) +
        geom_vline(
      xintercept=ano_intervencao,
      linetype="dashed"
    ) +
        scale_x_continuous(
      breaks = anos,
      name = NULL
    ) +
        scale_color_manual(values=c(
      "Observed"="black",
      "Univariate"="#1f78b4",
      "Multivariate"="#e31a1c"
    )) +
        scale_fill_manual(values=c(
      "Univariate"="#a6cee3",
      "Multivariate"="#fcae91"
    )) +
        labs(
      y="Age-standardized rate",
      color="Model specification",
      fill="Model specification"
    ) +
        annotate(
      "text",
      x = min(anos),
      y = max(df_regiao$rate)*1.05,
      label = regiao,
      hjust = 0,
      size = 5,
      fontface = "bold"
    ) +
        theme_minimal() +
    theme(legend.position="bottom")
  
   # PANEL 2 – POINTWISE EFFECT
    
  p2 <- ggplot() +
      geom_ribbon(
      data=g_uni,
      aes(x=ano,ymin=eff_low,ymax=eff_up,fill="Univariate"),
      alpha=.25
    ) +
      geom_line(
      data=g_uni,
      aes(x=ano,y=effect,color="Univariate"),
      linewidth=1
    ) +
      geom_ribbon(
      data=g_multi,
      aes(x=ano,ymin=eff_low,ymax=eff_up,fill="Multivariate"),
      alpha=.25
    ) +
      geom_line(
      data=g_multi,
      aes(x=ano,y=effect,color="Multivariate"),
      linewidth=1
    ) +
      geom_hline(yintercept=0,linetype="dotted") +
      geom_vline(
      xintercept=ano_intervencao,
      linetype="dashed"
    ) +
      scale_x_continuous(
      breaks = anos,
      name = NULL
    ) +
      labs(y="Pointwise effect") +
      theme_minimal() +
    theme(legend.position="none")
  
   # PANEL 3 – CUMULATIVE EFFECT
    
  p3 <- ggplot() +
      geom_ribbon(
      data=g_uni,
      aes(x=ano,ymin=cum_low,ymax=cum_up,fill="Univariate"),
      alpha=.25
    ) +
      geom_line(
      data=g_uni,
      aes(x=ano,y=cumul,color="Univariate"),
      linewidth=1
    ) +
      geom_ribbon(
      data=g_multi,
      aes(x=ano,ymin=cum_low,ymax=cum_up,fill="Multivariate"),
      alpha=.25
    ) +
      geom_line(
      data=g_multi,
      aes(x=ano,y=cumul,color="Multivariate"),
      linewidth=1
    ) +
      geom_hline(yintercept=0,linetype="dotted") +
      geom_vline(
      xintercept=ano_intervencao,
      linetype="dashed"
    ) +
      scale_x_continuous(
      breaks = anos,
      name = "Year"
    ) +
      labs(
      y="Cumulative effect"
    ) +
      theme_minimal() +
    theme(legend.position="none")
  
  grafico <- (p1/p2/p3)
  
  return(list(
    grafico = grafico,
    impact_uni = impact_uni,
    impact_multi = impact_multi
  ))
}

# RUN ANALYSIS FOR ALL REGIONS

regioes <- unique(dados_long$region)

lista_graficos <- list()
resultados <- list()

for(r in regioes){
  
  message("Running region: ", r)
  
  res <- analise_regiao(
    dados_long,
    r,
    2020
  )
  
  lista_graficos[[r]] <- res$grafico
  resultados[[r]] <- res
  
  cat("\n=================================================\n")
  cat("REGION:", r, "\n")
  cat("=================================================\n")
  
  cat("\nUNIVARIATE MODEL\n")
  print(summary(res$impact_uni))
  cat(summary(res$impact_uni, "report"), "\n")
  print(res$impact_uni$summary)
  
  cat("\nMULTIVARIATE MODEL\n")
  print(summary(res$impact_multi))
  cat(summary(res$impact_multi, "report"), "\n")
  print(res$impact_multi$summary)
}

# FINAL FIGURES

figura1 <-
  (lista_graficos[["North"]] |
     lista_graficos[["Northeast"]] |
     lista_graficos[["Southeast"]]) +
  
  plot_layout(guides = "collect") +
  
  plot_annotation(
    title = "Evolution of the Age-Standardized HIV Infection Rate in the Five Regions of Brazil",
    subtitle = "Comparison between univariate and multivariate counterfactual models"
  )

figura1 &
  theme(
    legend.position = "bottom",
    legend.title = element_blank()
  )

figura2 <-
  (lista_graficos[["South"]] |
     lista_graficos[["Midwest"]]) +
  
  plot_layout(guides = "collect") +
  
  plot_annotation(
    title = "Evolution of the Age-Standardized HIV Infection Rate in the Brazilian Regions (continued)",
    subtitle = "Comparison between univariate and multivariate counterfactual models"
  )

figura2 &
  theme(
    legend.position = "bottom",
    legend.title = element_blank()
  )
