# -*- coding: utf-8 -*-

# $Id:$

from django import forms
from django.contrib.auth.decorators import login_required
from django.contrib.auth.decorators import permission_required
from django.core.urlresolvers import reverse
from django.forms import widgets
from django.shortcuts import render_to_response
from django.shortcuts import redirect
from django.template import RequestContext
from django.utils.translation import ugettext_lazy as _
from django.views.generic import FormView
from django.views.generic import TemplateView

from bdfcee.homer import models as app_models


class _Form(forms.Form) :
    """
    """
    pass


class (>>>POINT<<<)(>>>FILE_UPCASE<<<)(FormView) :

    template_name = "homer/(>>>FILE_SANS<<<).html"
    form_class = _Form

    def form_valid(self, form) :
        """Processa les dades del formulari.
        """

        return redirect("(>>>FILE_SANS<<<)_ok")

    def get_context_data(self, **kwargs) :
        """Retorna el context de la plantilla.

        Retorna el context que se li passarà a la plantilla associada
        a la vista. Li afegeix la variable 'codi'.
        """
        kwargs["codi"] = self.kwargs["codi"]
        return kwargs



class (>>>FILE_UPCASE<<<)Ok(TemplateView) :
    """Mostra un missatge d'Ok.
    """
    template_name = "homer/(>>>FILE_SANS<<<)_ok.html"


