
### creating custom theme object
sisinta_theme <- dashboardthemes::shinyDashboardThemeDIY(

  ### general
  appFontFamily = "Arial"
  ,appFontColor = "rgb(0,0,0)"
  # ,primaryFontColor = "rgb(0,97,158)"
  ,primaryFontColor = "rgb(255,255,255)"
  ,infoFontColor = "rgb(255,255,255)"
  ,successFontColor = "rgb(255,255,255)"
  ,warningFontColor = "rgb(255,255,255)"
  ,dangerFontColor = "rgb(255,255,255)"
  ,bodyBackColor = "rgb(255,255,255)"

  ### header
  ,logoBackColor = "rgb(96,150,179)"

  ,headerButtonBackColor = "rgb(96,150,179)"
  ,headerButtonIconColor = "rgb(0,85,121)"
  ,headerButtonBackColorHover = "rgb(0,85,121)"
  ,headerButtonIconColorHover = "rgb(96,150,179)"

  ,headerBackColor = "rgb(96,150,179)"
  ,headerBoxShadowColor = "#aaaaaa"
  ,headerBoxShadowSize = "2px 2px 2px"

  ### sidebar
  ,sidebarBackColor = "rgb(0,85,121)"
  ,sidebarPadding = 5

  ,sidebarMenuBackColor = "transparent"
  ,sidebarMenuPadding = 0
  ,sidebarMenuBorderRadius = 0

  ,sidebarShadowRadius = "3px 5px 5px"
  ,sidebarShadowColor = "#aaaaaa"

  ,sidebarUserTextColor = "rgb(255,255,255)"

  ,sidebarSearchBackColor = "rgb(55,72,80)"
  ,sidebarSearchIconColor = "rgb(153,153,153)"
  ,sidebarSearchBorderColor = "rgb(55,72,80)"

  ,sidebarTabTextColor = "rgb(255,255,255)"
  ,sidebarTabTextSize = 13
  ,sidebarTabBorderStyle = "none none solid none"
  ,sidebarTabBorderColor = "rgb(96,150,179)"
  ,sidebarTabBorderWidth = 1

  ,sidebarTabBackColorSelected = "rgb(96,150,179)"
  ,sidebarTabTextColorSelected = "rgb(255,255,255)"
  ,sidebarTabRadiusSelected = "0px 20px 20px 0px"

  ,sidebarTabBackColorHover = "rgb(255,255,255)"
  ,sidebarTabTextColorHover = "rgb(50,50,50)"
  ,sidebarTabBorderStyleHover = "none none solid none"
  ,sidebarTabBorderColorHover = "rgb(255,255,255)"
  ,sidebarTabBorderWidthHover = 5
  ,sidebarTabRadiusHover = "0px 20px 20px 0px"

  ### boxes
  ,boxBackColor = "rgb(255,255,255)"
  ,boxBorderRadius = 5
  ,boxShadowSize = "0px 1px 1px"
  ,boxShadowColor = "rgba(0,0,0,.1)"
  ,boxTitleSize = 16
  ,boxDefaultColor = "rgb(0,97,158)"
  ,boxPrimaryColor = "rgba(44,222,235,1)"
  ,boxInfoColor = "rgb(210,214,220)"
  ,boxSuccessColor = "rgba(0,255,213,1)"
  ,boxWarningColor = "rgb(244,156,104)"
  ,boxDangerColor = "rgb(255,88,55)"

  ,tabBoxTabColor = "rgb(255,255,255)"
  ,tabBoxTabTextSize = 14
  ,tabBoxTabTextColor = "rgb(255,255,255)"
  ,tabBoxTabTextColorSelected = "rgb(255,255,255)"
  ,tabBoxBackColor = "rgb(255,255,255)"
  ,tabBoxHighlightColor = "rgba(44,222,235,1)"
  ,tabBoxBorderRadius = 5

  ### inputs
  ,buttonBackColor = "rgb(96,150,179)"
  ,buttonTextColor = "rgb(255,255,255)"
  ,buttonBorderColor = "rgb(96,150,179)"
  ,buttonBorderRadius = 5
  ,buttonPadding = "6px 12px"

  ,buttonBackColorHover = "rgb(255,255,255)"
  ,buttonTextColorHover = "rgb(0,85,121)"
  ,buttonBorderColorHover = "rgb(255,255,255)"

  ,textboxBackColor = "rgb(255,255,255)"
  ,textboxBorderColor = "rgb(200,200,200)"
  ,textboxBorderRadius = 5
  ,textboxBackColorSelect = "rgb(245,245,245)"
  ,textboxBorderColorSelect = "rgb(200,200,200)"

  ### tables
  ,tableBackColor = "rgb(255,255,255)"
  ,tableBorderColor = "rgb(240,240,240)"
  ,tableBorderTopSize = 1
  ,tableBorderRowSize = 1

)



