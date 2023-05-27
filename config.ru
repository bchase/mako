require 'pathname'

html = Pathname.new('./index.html').read

app = proc do |env|
  [200, {'content-type' => 'text/html'}, [html]]
end

run app
