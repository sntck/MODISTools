GetProducts <-
function()
{
  ornlMODIS = processWSDL('http://daac.ornl.gov/cgi-bin/MODIS/GLBVIZ_1_Glb_subset/MODIS_webservice.wsdl')
  ornlMODISFuncs = genSOAPClientInterface(operations=ornlMODIS@operations[[1]], def=ornlMODIS)
  ornlMODISFuncs@functions$getproducts()
}

