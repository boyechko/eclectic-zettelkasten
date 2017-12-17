# -----------------------------------------------------------------------------
#      Author: Richard Boyechko <rb-mercurial@diachronic.net>
# Description: A collection of methods for working with Zettel
#     Created: 2016-09-02
#    Comments:
# -----------------------------------------------------------------------------

require 'time'
require 'yaml'
require 'pathname'

#-------------------------------------------------------------------------------
# Zettelkaesten
#-------------------------------------------------------------------------------

class Zettelkaesten
  class << self; attr_accessor :root end
  class << self; attr_accessor :ext end

  # Default extension for Zettelkaesten files
  @ext = ".txt"

  @root = Pathname(ENV['ZETTEL_DIR'] || File.expand_path('~/Dropbox/Zettel'))
  @kaesten = { "main"    => root + "main",
               "limbo"   => root + "limbo",
               "tech"    => root + "tech",
               "writing" => root + "writing" }

  # Returns the directory for the given kasten
  def self.dir(kasten)
    if @kaesten[kasten]
      return @kaesten[kasten]
    else
      raise "Unknown kasten '#{kasten}'"
    end
  end

  # Returns true if the given string names a valid kasten
  def self.kasten?(string)
    return true if @kaesten[string]
  end

  # Returns the Kasten that the Zettel path is under
  def self.kasten_of(path)
    p = Pathname(path)
    p = Pathname.pwd() + p unless p.absolute?
    relv = p.relative_path_from(@root)
    kasten = relv.each_filename.to_a[0]
    return kasten if @kaesten[kasten]
  end

  # Returns the type of the zettel found at the given path
  def self.zettel_type(path)
    kasten_of(path) == "main" ? :numerus : :tempus
  end

  # Returns true if the given path is in the Zettelkaesten
  def self.includes?(path)
    return true if kasten_of(path)
  end

  # Returns the appropriate Zettel subclass (Numerus or Tempus) of the Zettel at
  # the given path.
  def self.zettel_at(path)
    kasten = kasten_of(path)
    if kasten == "main"
      return Numerus.new_from_path(path)
    elsif kasten
      return Tempus.new_from_path(path)
    else
      raise "The file at given path is not a Zettel: #{path}"
    end
  end

  # Returns the appropriate Zettel subclass (Numerus or Tempus) of the Zettel
  # from the given link.
  def self.zettel_from_link(link)
    if Numerus.valid_link?(link)
      return Numerus.new_from_link(link)
    elsif Tempus.valid_link?(link)
      return Tempus.new_from_link(link)
    else
      return nil
    end
  end

  # Returns the appropriate Zettel subclass (Numerus or Tempus) of the Zettel
  # from the given path.
  def self.zettel_from_path(path)
    if Numerus.valid_path?(path)
      return Numerus.new_from_path(path)
    elsif Tempus.valid_path?(path)
      return Tempus.new_from_path(path)
    else
      return nil
    end
  end
end

#-------------------------------------------------------------------------------
# Zettel
#-------------------------------------------------------------------------------

class Zettel
  attr_reader :type,            # Zettel type; either :tempus or :numerus
              :kasten,          # Kasten, as string
              :slug,            # slug only (i.e. without Kasten)
              :link,            # full link (i.e. with Kasten, unless main)
              :path             # full path, as Pathname
  attr_accessor :metadata       # hash

  def initialize_metadata()
    if File.exists?(@path)
      begin
        @metadata = YAML::load(File.read(path).split("\n\n", 2)[0])
      rescue Exception => e
        raise "Malformed metadata: #{e.message}"
      end
    end
  end

  # Returns true if the zettel exists where it should
  def exists?
    File.exists?(@path)
  end

  # Returns a relative path to Zettel from the Zettelkaesten root
  def relative_path()
    if @path
      return @path.relative_path_from(Zettelkaesten.root)
    else
      raise "The Zettel has no path set"
    end
  end

  # Generates a YAML block as a string, using inline sequence style. Can't use
  # YAML::to_yaml() because it does not support inline style.
  def self.to_yaml()
    result = ""
    hash.each do |key, val|
      if val.is_a?(Array)
        result += "#{key}: [ #{val.join(', ')} ]\n"
      else
        result += "#{key}: #{val}\n"
      end
    end
    return result
  end
end

class Numerus < Zettel
  SLUG_PATTERN = /^([0-9]{3})(-([a-z]+))*$/
  LINK_PATTERN = SLUG_PATTERN

  attr_reader :numerus,         # the number portion of the slug
              :litterae,        # the letter portion of thes lug
              :section          # the section of the main Kasten

  #
  # Custom Constructors
  #
  def self.new_from_path(*args)
    numerus = allocate
    numerus.init_path(*args)
    numerus
  end

  def self.new_from_link(*args)
    numerus = allocate
    numerus.init_link(*args)
    numerus
  end

  #
  # Custom Initializers
  #
  def init_path(path)
    if Zettelkaesten.includes?(path)
      init_link(Pathname(path).basename(Zettelkaesten.ext).to_s)
    else
      raise "The path is not part of the Zettelkaesten: #{path}"
    end
  end

  def init_link(link)
    if link =~ LINK_PATTERN
      @slug = link
      @type = :numerus
      @kasten = "main"
      @numerus = $1.to_i
      @litterae = $3
      @section = self.class.section_of(@slug)
      @path = Zettelkaesten.dir(@kasten) + @section + (@slug + Zettelkaesten.ext)
      initialize_metadata if @path.exist?
    else
      raise "The link does not point to a numerus currens: #{link}"
    end
  end

  #
  # Instance Methods
  #

  # Returns the wiki link target
  def link()
    return @slug
  end

  #
  # Class Methods
  #

  # Returns the appropriate sub-directory in the main Kasten based on the Zettel
  # slug.
  def self.section_of(slug)
    if slug =~ SLUG_PATTERN
      num = $1.to_i
      if num >= 0 and num <= 99
        return "000-099"
      elsif num >= 100 and num <= 999
        return "#{slug[0]}00-#{slug[0]}99"
      else
        # Should never get here: SLUG_PATTERN limits the numerus to three digits
        raise "Numerus currens '#{slug}' is out of bounds (0-999)"
      end
    else
      raise "Slug '#{slug}' is not a numerus currens"
    end
  end

  # Returns true if the link (a string) is a valid link to Numerus Currens Zettel
  def self.valid_link?(string)
    return string =~ LINK_PATTERN ? true : false
  end

  # Returns true if the string is a valid path a to numerus currens zettel
  def self.valid_path?(string)
    # FIXME: Hardcoded main kasten path pattern
    return File.basename(string, Zettelkaesten.ext) =~ SLUG_PATTERN &&
           File.dirname(string) =~ /main\/\d{3}-\d{3}$/ ? true : false
  end
end

#-------------------------------------------------------------------------------
# Tempus
#-------------------------------------------------------------------------------

class Tempus < Zettel
  LINK_PATTERN = /^([a-z]+):(\d{8}T\d{4})$/
  SLUG_PATTERN = /^\d{8}T\d{4}$/

  attr_reader :time             # the time of the Zettel as a Time object

  #
  # Custom Constructors
  #
  def self.new_from_path(*args)
    numerus = allocate
    numerus.init_path(*args)
    numerus
  end

  def self.new_from_link(*args)
    numerus = allocate
    numerus.init_link(*args)
    numerus
  end

  #
  # Custom Initializers
  #
  def init_path(path)
    if Zettelkaesten.includes?(path)
      init_link(Zettelkaesten.kasten_of(path) +
                ":" +
                Pathname(path).basename(Zettelkaesten.ext).to_s)
    else
      raise "The path is not part of the Zettelkaesten: #{path}"
    end
  end

  def init_link(link)
    if link =~ LINK_PATTERN
      @slug = $2
      @type = :tempus
      @kasten = $1
      @time = Time.parse(@slug)
      @path = Zettelkaesten.dir(@kasten) + (@slug + Zettelkaesten.ext)
      initialize_metadata if @path.exist?
    else
      raise "The link does not point to a Tempus Zettel: #{link}"
    end
  end

  #
  # Instance Methods
  #

  # Returns the wiki link target
  def link()
    return "#{@kasten}:#{@slug}"
  end

  #
  # Class Methods
  #

  # Returns true if the link (a string) is a valid link to Tempus Zettel
  def self.valid_link?(string)
    return string =~ LINK_PATTERN ? true : false
  end

  # Returns true if this is a valid path a to numerus currens zettel
  def self.valid_path?(string)
    return File.basename(string, Zettelkaesten.ext) =~ SLUG_PATTERN &&
           Zettelkaesten.includes?(string) ? true : false
  end
end
