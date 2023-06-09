#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(bslib)
library(shinycustomloader)
library(dplyr)
library(tidyr)

##### Javascript that adds Title attribute to options when they're added as an option or selected; titles being used for tooltips
# create dictionary of descriptions
group_descriptions <- sesame::KYCG_listDBGroups() %>% tidyr::extract(col=Title,regex ="^(\\w+).(\\w+).(.+)\\.(\\w+)$",into=c("kycg","platform","group","group_suffix"),remove=FALSE) %>% unite("platform_group",platform:group,sep = ".") %>% select(platform_group,Description)

group_descriptions$pair <- paste(paste("'",group_descriptions$platform_group,"'",sep = ""),paste("'",group_descriptions$Description,"'",sep=""),sep=":")
descriptions_dict<- paste(group_descriptions$pair,collapse = ",")
descriptions_dict<- paste("{", descriptions_dict,"}", sep="")

js_descriptions <- paste("descriptions_dict=",descriptions_dict,";
$('div:has(#selected_groups)').arrive('div.item', function() {
    var $newElem = $(this);
    console.log($newElem.attr('data-value'));
	var group = $newElem.attr('data-value');
	var platform = $('#selected_platform >option').attr('value');
	var pg_key = platform +'.' +group;
	var description = descriptions_dict[pg_key];
    $newElem.attr('title',description);
});

$('div:has(#selected_groups)').arrive('div.option', function() {
    var $newElem = $(this);
    console.log($newElem.attr('data-value'));
	var group = $newElem.attr('data-value');
	var platform = $('#selected_platform >option').attr('value');
	var pg_key = platform +'.' +group;
	var description = descriptions_dict[pg_key];
    $newElem.attr('title',description);
});
")

first_bs4_theme <-bs_theme(version = 4)
# define CHOP header html
header = "<header>
	<nav class='navbar-default navbar-expand-sm margin-bottom-50' id='site-nav' role='navigation'>
		<div class='container-fluid'>
			<div class='row'>
				<div class='col-12 col-sm-4'>
					<div class='navbar-header'>
						<div class='brand'>
							<div class='choplogodiv text-center text-sm-left'>
								<a class='navbar-brand chop-logo ' href='/'>
									<img src='https://assets.research.chop.edu/images/logo_color_chopri.svg'>
									<span class='element-invisible'>Research Institute at The Children's Hospital of Philadelphia</span>
								</a>
							</div>
						</div>
					</div>
				</div>
				<div class='col-12 col-sm-8'>
					<div class='row'>
						<div class='col-lg-12 col-md-12 col-12 siteName text-center text-sm-right mt-0  mt-sm-3 '>
							KnowYourCG
						</div>
					</div>
				</div>
			</div>

		</div>
	</nav>
</header>"

# define CHOP footer html
footer <- "<footer class='border-top text-muted '>
            <div id='chop-branded-footer' style='clear:both; text-align:left !important;padding-left:5px;padding-top:5px; padding-bottom:5px;color:white !important;'>
    <div class='row'>
        <div class='col-12 col-lg-12'>
            <a href='https://www.research.chop.edu' style='border-bottom:0 !important;'><img id='chop-footer-logo' src='https://assets.research.chop.edu/images/chop_ri_logo.svg' style='border:0 !important; width:200px; height:auto;background:transparent !important; box-shadow:transparent !important; padding-left:0 !important; margin-left:0 !important;float:none !important;' alt='CHOP Research Institute' class='img-responsive'></a>
        </div>
    </div>
    <br>
    <div class='row'>
        <div class='col-12 col-lg-12 text-left' style='text-align:left !important;'>
            <p class='text-left' style='text-align:left !important; margin:0;padding:0 !important;'>
                3401 Civic Center Blvd.<br>
                Philadelphia, PA 19104
            </p>
        </div>
    </div>
    <br>
    <div class='row'>
        <div class='col-12 col-lg-12 text-left' style='text-align:left !important;'>
            <p class='text-left' style='text-align:left !important;margin:0;padding:0 !important;'>© <!--?php echo date('Y'); ?--> <a href='http://www.chop.edu' title='link to Children\'s Hospital of Philadelphia web site'>Children's Hospital of Philadelphia</a>. All Rights Reserved.</p>
  </div>
  </div>
  <div class='row'>
  <div class='col-12 col-lg-12 text-left' style='text-align:left !important;' id='footer-links'>
  <p style='margin:0;padding:0 !important;'><a href='http://www.chop.edu/pages/privacy-policy' title='link to CHOP Privacy Policy'>Privacy Policy</a> | <a href='https://www.chop.edu/pages/terms-use' title='link to CHOP Research Institute Terms of Use'>Terms of Use</a> | <a href='https://www.chop.edu/patients-and-visitors/protecting-patient-privacy'>HIPAA Notice of Privacy Practices</a> | <a href='https://www.chop.edu/about-us/ethics-and-compliance'>Ethics &amp; Compliance</a></p>
  </div>
  </div>
  </div>
  
  <div class='container'>
  <div class='row'>
  <div class='col-sm-12'>
  </div>
  </div>
  </div>
  </footer>"

intro <- "<div class='offset-1 pb-2'><em>An automated discovery tool for discovering hidden biological and technical links</em></div>
<h4 class='offset-1'>What is KnowYourCG?</h4>
<div class='row offset-1 mb-3'>
    <div class=' col-12 col-md-11 pl-0'><p> KnowYourCG is a computational tool for automated discovery of the functional implications of DNA methylation data by scanning CpG sets in large number of knowledgebases for set enrichment, investigating association with quantitative measurements, and looking for highly correlated CpG subsets (modules).</p> </div>

</div>"


# Define UI for application 
fluidPage(theme=first_bs4_theme, class="px-0",
          tags$title("KnowYourCG "),
          tags$head(
            tags$link(rel="shortcut icon", href="favicon.ico"),
            tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
            tags$script(src = "https://kit.fontawesome.com/0c5700dea2.js"),
            tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/arrive/2.4.1/arrive.min.js"),
            ),
    includeHTML("google-analytics.html"),

    div(HTML(header)),
    tags$h2("Welcome to KnowYourCG",class="welcome-msg offset-1"),
    HTML(intro),
    div(class="row",
        div(class="col-10 offset-1 blue-light mb-3",
            div(class="row",
                div(class="col",
                    tags$h3("Define Query Set")
                    )
                ),
            div(class="row",
                div(class="col-md-5 col-11 ml-3",
                    div(class="row",
                        #tags$h3("Enter CPG IDs")
                        ),
                    div(class="row",
                        textAreaInput("query", "Enter CPG IDs","", 
                                      placeholder = "Paste a set of CpG IDs below (one on each row)", 
                                      width = "300px",
                                      rows=6),
                    ),                        
                    div(class="row ml-0",
                        #actionButton("tryExampleButton", "Try a query set example.",class="mb-3")
                        tags$a("Try a query set example.",href="#",onclick="Shiny.setInputValue(\"link_show_example\", true, {priority: \"event\"})")
                    )
                    
                  ),
                div(class="offset-4 offset-md-0 col-12 col-md-1 text-center",
                    div(class="row pt-4",
                        tags$br(),
                        tags$h4("OR")
                        ),
                    div(class="row",
                        div(class="p-0 or-column border-right"),
                        div(class="p-0 or-column")
                        )
                    ),
                div(class="col-md-5 col-11 ml-3 ml-md-1",
                    div(class="row",
                        #tags$h3("Upload CPG IDs")
                    ),
                    div(class="row",
                        fileInput("uploaded_cpg_ids",
                                  label = "Upload CPG IDs from Text File",accept = c(".csv",".txt"))
                    ),
                    div(class="row",
                        downloadLink("example_file_download",
                                     HTML('<i class="fa fa-download"></i>Download Example File'),
                                       " Download Example File")
                    )
                )
                ),
            div(class="row", id="refineDetails",
                div(class="col col-md-6 mt-4 mt-md-0",
                    selectizeInput("selected_platform","Select a platform", choices = NULL,
                                   options = list(
                                     placeholder = 'Please select a platform',
                                     onInitialize = I('function() { this.setValue(""); }')
                                   )),
                    div(class="msg-inferred",
                    textOutput("inferredPlatformMsg")),
                    div(class="msg-not-inferred",
                    textOutput("notInferredPlatformMsg"))
                    ),
                div(class="col col-md-6",
                    selectInput("selected_groups",  label = tags$span("Database Groups", tags$i(
                        class = "fa-solid fa-circle-info", 
                        style = "color:#0072B2;",
                        title = "Defaults to common groups "
                      )),choices = NULL, multiple = TRUE),
                    checkboxInput("geneAssociation", "Gene association", FALSE),
                    
                    
                )
                ),
            div(class="row",
                div(class="col",
                    actionButton("submitBUtton", "Submit!",class="btn-primary mb-3"),
                    tags$br()                  
                )
                )
            )
        ),
    fluidRow(
      div(class="col-10 offset-1",
                    conditionalPanel("input.submitBUtton > 0", 
                            tags$h3("Results",
                                    tags$span(class="small",downloadLink("downloadData",
                                                                                      HTML('<i class="fa fa-download"></i>')
                                                                                      ))
                                    ),
                            
                            tags$h4("Overview Dotplot",
                                    tags$span(class="small",downloadLink("downloadDataOverview",
                                                                                      HTML('<i class="fa fa-download"></i>')
                                                                                                      
                                                                                                      ))
                                    ),
                           withLoader(ggiraph::girafeOutput("distPlot", width="100%",height = "500px")))          
          ),
      ),

    fluidRow(
      div(class="col-10 offset-1",
          conditionalPanel("input.submitBUtton > 0", 
                           tags$h4("Bygroup Dotplots",
                                   tags$span(class="small",downloadLink("downloadDataByGroup",
                                                                        HTML('<i class="fa fa-download"></i>')
                                                                        
                                   ))
                                   ),
                           div(class="row",
                               div(class="col-4, offset-1",
                                   uiOutput("dynamic_tabs")
                               ),
                               div(class="col-6",
                                   withLoader(ggiraph::girafeOutput("dotPlot", width="100%",height = "500px"))
                               ))
          )
      )
    ),
    HTML(footer),
    tags$script(HTML(js_descriptions)),
)
