require 'pathname'

html = Pathname.new('./index.html').read

app = proc do |env|
  [200, {'Content-Type' => 'text/html'}, [html]]
end

run app
